use petgraph::{graph::NodeIndex, Graph};
use serde::Serialize;
use solver::backend::{sabre_solve, sabre_solve_parallel, solve, solve_joint_optimize, solve_joint_optimize_parallel, solve_with_cached_heuristic};
use solver::structures::*;
use solver::utils::Move;
use std::collections::{HashMap, HashSet};
#[derive(Clone)]
pub struct NisqArchitecture {
    graph: Graph<Location, ()>,
    index_map: HashMap<Location, NodeIndex>,
}
impl NisqArchitecture {
    pub fn new(graph: Graph<Location, ()>) -> Self {
        let mut index_map = HashMap::new();
        for ind in graph.node_indices() {
            index_map.insert(graph[ind], ind);
        }
        return NisqArchitecture { graph, index_map };
    }
    pub fn get_graph(&self) -> &Graph<Location, ()> {
        return &self.graph;
    }
}

impl Architecture for NisqArchitecture {
    fn locations(&self) -> Vec<Location> {
        let mut locations = Vec::new();
        for node in self.graph.node_indices() {
            locations.push(self.graph[node]);
        }
        return locations;
    }
    fn graph(&self) -> (Graph<Location, ()>, HashMap<Location, NodeIndex>) {
        return (self.graph.clone(), self.index_map.clone());
    }
}

fn swap_on_edge(
    map: &HashMap<Qubit, Location>,
    locs: (Location, Location),
) -> HashMap<Qubit, Location> {
    let mut new_map = map.clone();
    for (qubit, loc) in map {
        if loc == &locs.0 {
            new_map.insert(*qubit, locs.1);
        } else if loc == &locs.1 {
            new_map.insert(*qubit, locs.0);
        }
    }
    return new_map;
}
#[derive(Debug)]
struct NisqTrans {
    edge: (Location, Location),
}
#[derive(Clone, Debug, Serialize, Hash, PartialEq, Eq)]
pub struct NisqGateImplementation {
    edge: (Location, Location),
}

impl GateImplementation for NisqGateImplementation {}

type NisqStep = Step<NisqGateImplementation>;

impl Transition<NisqGateImplementation, NisqArchitecture> for NisqTrans {
    fn apply(&self, step: &NisqStep) -> NisqStep {
        let mut new_step = step.clone();
        new_step.map = swap_on_edge(&step.map, self.edge);
        new_step.implemented_gates = HashSet::new();
        return new_step;
    }
    fn repr(&self) -> String {
        return format!("{:?}", self);
    }

    fn cost(&self, _arch : &NisqArchitecture) -> f64 {
        if self.edge.0 == self.edge.1 {
            0.0
        } else {
            1.0
        }
    }
}

fn nisq_transitions(step: &NisqStep, arch: &NisqArchitecture) -> Vec<NisqTrans> {
    let mut transitions = Vec::new();
    transitions.push(NisqTrans {
        edge: (Location::new(0), Location::new(0)),
    });
    for edge in arch.graph.edge_indices() {
        let (source, target) = arch.graph.edge_endpoints(edge).unwrap();
        let (loc1, loc2) = (arch.graph[source], arch.graph[target]);
        if step.map.values().collect::<Vec<_>>().contains(&&loc1) || step.map.values().collect::<Vec<_>>().contains(&&loc1) {
                let trans = NisqTrans { edge: (loc1, loc2) };
                transitions.push(trans);
        }
    }
    return transitions;
}

fn nisq_implement_gate(
    step: &NisqStep,
    arch: &NisqArchitecture,
    gate: &Gate,
) -> Vec<NisqGateImplementation> {
    let graph = arch.get_graph();
    let (cpos, tpos) = (step.map.get(&gate.qubits[0]), step.map.get(&gate.qubits[1]));
    match (cpos, tpos) {
        (Some(cpos), Some(tpos))
            if graph.contains_edge(arch.index_map[cpos], arch.index_map[tpos]) =>
        {
            vec![NisqGateImplementation {
                edge: (*cpos, *tpos),
            }]
        }
        _ => vec![],
    }
}

fn nisq_step_cost(_step: &NisqStep, _arch: &NisqArchitecture) -> f64 {
    0.0
}

fn mapping_heuristic(arch: &NisqArchitecture, c: &Circuit, map: &HashMap<Qubit, Location>) -> f64 {
    let graph = arch.get_graph();
    let mut cost = 0;
    for gate in &c.gates {
        let (cpos, tpos) = (map.get(&gate.qubits[0]), map.get(&gate.qubits[1]));
        let (cind, tind) = (arch.index_map[cpos.unwrap()], arch.index_map[tpos.unwrap()]);
        let sp_res = petgraph::algo::astar(graph, cind, |n| n == tind, |_| 1, |_| 0);

        match sp_res {
            Some((c, _)) => {cost += c;
                //  println!("gate: {:?}, distance {:?}", gate, c)
                 }
            None => panic!(
                "Disconnected graph. No path found from {:?} to {:?}",
                cpos, tpos
            ),
        }
    }
    return cost as f64;
}

fn delta_on_move(map: &QubitMap, chosen_move: Move, c: &Circuit, arch: &NisqArchitecture) -> f64 {
    let mut delta = 0;
    let graph = arch.get_graph();
    let mut new_map = map.clone();
    let mut moved_qubits = vec![];
    match chosen_move {
        Move::Swap(q1, q2) => {
            let loc1 = map.get(&q1).unwrap();
            let loc2 = map.get(&q2).unwrap();
            new_map.insert(q1, *loc2);
            new_map.insert(q2, *loc1);
            moved_qubits.push(q1);
            moved_qubits.push(q2);
        }
        Move::IntoOpen(qubit, location) => {
            new_map.insert(qubit, location);
            moved_qubits.push(qubit);
        }
    }
    for gate in &c.gates {
        let modified = moved_qubits.iter().any(|x| gate.qubits.contains(x));
        if modified {
            let (cpos_old, tpos_old) = (map.get(&gate.qubits[0]), map.get(&gate.qubits[1]));
            let (cind_old, tind_old) = (
                arch.index_map[cpos_old.unwrap()],
                arch.index_map[tpos_old.unwrap()],
            );
            let sp_res_old =
                petgraph::algo::astar(graph, cind_old, |n| n == tind_old, |_| 1, |_| 0);
            let (cpos_new, tpos_new) = (new_map.get(&gate.qubits[0]), new_map.get(&gate.qubits[1]));
            let (cind_new, tind_new) = (
                arch.index_map[cpos_new.unwrap()],
                arch.index_map[tpos_new.unwrap()],
            );
            let sp_res_new =
                petgraph::algo::astar(graph, cind_new, |n| n == tind_new, |_| 1, |_| 0);
            match (sp_res_new, sp_res_old) {
                (None, None) => panic!("disconnected graph in computing mapping heuristic"),
                (None, Some(_)) => panic!("disconnected graph in computing mapping heuristic"),
                (Some(_), None) => panic!("disconnected graph in computing mapping heuristic"),
                (Some((c_new, _)), Some((c_old, _))) => delta += c_new - c_old,
            }
        }
    }
    return delta as f64;
}

pub fn nisq_solve_sabre(
    c: &Circuit,
    a: &NisqArchitecture,
) -> CompilerResult<NisqGateImplementation> {
    return sabre_solve(
        c,
        a,
        &|s| nisq_transitions(s, a),
        &nisq_implement_gate,
        nisq_step_cost,
        Some(mapping_heuristic),
        false,
    );
}

pub fn nisq_solve_sabre_par(
    c: &Circuit,
    a: &NisqArchitecture,
) -> CompilerResult<NisqGateImplementation> {
    return sabre_solve_parallel(
        c,
        a,
        &|s| nisq_transitions(s, a),
        &nisq_implement_gate,
        nisq_step_cost,
        Some(mapping_heuristic),
        false,
    );
}
pub fn nisq_solve(c: &Circuit, a: &NisqArchitecture) -> CompilerResult<NisqGateImplementation> {
    return solve(
        c,
        a,
        &|s| nisq_transitions(s, a),
        &nisq_implement_gate,
        nisq_step_cost,
        Some(mapping_heuristic),
        false,
    );
}

pub fn nisq_solve_cached_heuristic(c: &Circuit, a: &NisqArchitecture) -> CompilerResult<NisqGateImplementation> {
    return solve_with_cached_heuristic(
        c,
        a,
        &|s| nisq_transitions(s, a),
        nisq_implement_gate,
        nisq_step_cost,
        Some(mapping_heuristic),
        |map, mv| delta_on_move(map, mv, c, a),
        false,
    );
}

pub fn nisq_solve_joint_optimize(c: &Circuit, a: &NisqArchitecture) -> CompilerResult<NisqGateImplementation> {
    return solve_joint_optimize(
        c,
        a,
        &|s| nisq_transitions(s, a),
        nisq_implement_gate,
        nisq_step_cost,
        Some(mapping_heuristic),
        false,
        0
    );
}

pub fn nisq_solve_joint_optimize_parallel(c: &Circuit, a: &NisqArchitecture) -> CompilerResult<NisqGateImplementation> {
    return solve_joint_optimize_parallel(
        c,
        a,
        &|s| nisq_transitions(s, a),
        nisq_implement_gate,
        nisq_step_cost,
        Some(mapping_heuristic),
        false,
    );
}

pub fn nisq_solve_joint_optimize_parallel_no_opt(c: &Circuit, a: &NisqArchitecture) -> CompilerResult<NisqGateImplementation> {
    return solve_joint_optimize_parallel(
        c,
        a,
        &|s| nisq_transitions(s, a),
        nisq_implement_gate,
        nisq_step_cost,
        Some(mapping_heuristic),
        true,
    );
}
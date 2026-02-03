use crate::config::CONFIG;
use crate::structures::*;
use crate::utils::*;
use itertools::Itertools;
use petgraph::graph::NodeIndex;
use rand::seq::IndexedRandom;
use rayon::prelude::*;
use signal_hook::consts::{SIGINT, SIGTERM};
use signal_hook::flag;
use std::collections::HashSet;
use std::io::Write;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;
use std::time::Instant;
use std::{collections::HashMap, fmt::Debug};

fn random_map<T: Architecture>(c: &Circuit, arch: &T) -> QubitMap {
    let mut map = HashMap::new();
    let mut rng = &mut rand::rng();
    let locations = arch.locations();
    let v = locations.choose_multiple(&mut rng, c.qubits.len());
    for (q, l) in c.qubits.iter().zip(v) {
        map.insert(*q, *l);
    }
    return map;
}

fn isomorphism_map<T: Architecture>(c: &Circuit, arch: &T) -> Option<QubitMap> {
    let interact_graph = build_interaction_graph(c);
    let (mut graph, _) = arch.graph();
    if arch.locations().len() < arch.graph().0.node_count() {
        graph = reduced_graph(arch)
    }
    let isom = vf2::subgraph_isomorphisms(&interact_graph, &graph).first();
    isom.map(|v| {
        v.iter()
            .enumerate()
            .map(|(q, i)| (interact_graph[NodeIndex::new(q)], graph[NodeIndex::new(*i)]))
            .collect()
    })
}

fn _isomorphism_map_with_timeout<T: Architecture + Send + Sync + Clone + 'static>(
    c: &Circuit,
    arch: &T,
    timeout: Duration,
) -> Option<QubitMap> {
    let (tx, rx) = std::sync::mpsc::channel();
    let c_clone = c.clone();
    let arch_clone = arch.clone();
    thread::spawn(move || {
        let result = isomorphism_map(&c_clone, &arch_clone);
        let _ = tx.send(result);
    });

    match rx.recv_timeout(timeout) {
        Ok(res) => res,
        Err(_) => None,
    }
}

fn randomly_extend_partial_map<T: Architecture>(c: &Circuit, arch: &T, map: &QubitMap) -> QubitMap {
    let mut extended = map.clone();
    let mut rng = &mut rand::rng();
    let unmapped_qubits: Vec<_> = c.qubits.iter().filter(|q| !map.contains_key(q)).collect();
    let available_locations: Vec<_> = arch
        .locations()
        .into_iter()
        .filter(|v| !map.values().contains(v))
        .collect();
    let chosen_locations = available_locations.choose_multiple(&mut rng, c.qubits.len());
    for (q, l) in unmapped_qubits.iter().zip(chosen_locations) {
        extended.insert(**q, *l);
    }
    return extended;
}

fn incremental_isomorphism_map<T: Architecture>(c: &Circuit, arch: &T) -> Option<QubitMap> {
    let mut gates = &c.gates[..1];
    let mut prefix_circuit = circuit_from_gates(gates);
    let mut isom_map = None;
    let mut candidate = isomorphism_map(&prefix_circuit, arch);
    let mut i = 1;
    while candidate.is_some() && i < c.gates.len() {
        gates = &c.gates[..i];
        prefix_circuit = circuit_from_gates(gates);
        candidate = isomorphism_map(&prefix_circuit, arch);
        if candidate.is_some() {
            let full_map = candidate
                .clone()
                .map(|m| randomly_extend_partial_map(c, arch, &m));
            isom_map = full_map;
        }
        i += 1;
    }
    return isom_map;
}

fn incremental_isomorphism_map_with_timeout<T: Architecture + Send + Sync + Clone + 'static>(
    c: &Circuit,
    arch: &T,
    timeout: Duration,
) -> Option<QubitMap> {
    let (tx, rx) = std::sync::mpsc::channel();
    let c_clone = c.clone();
    let arch_clone = arch.clone();
    thread::spawn(move || {
        let result = incremental_isomorphism_map(&c_clone, &arch_clone);
        let _ = tx.send(result);
    });

    match rx.recv_timeout(timeout) {
        Ok(res) => res,
        Err(_) => None,
    }
}

fn random_neighbor<T: Architecture>(map: &QubitMap, arch: &T) -> QubitMap {
    let mut moves: Vec<Box<dyn Fn(&QubitMap) -> QubitMap>> = Vec::new();
    for q1 in map.keys() {
        for q2 in map.keys() {
            if q1 == q2 {
                continue;
            }
            let swap_keys = |m: &QubitMap| {
                let mut new_map = m.clone();
                let loc1 = m.get(q1).unwrap();
                let loc2 = m.get(q2).unwrap();
                new_map.insert(*q1, *loc2);
                new_map.insert(*q2, *loc1);
                return new_map;
            };
            moves.push(Box::new(swap_keys));
        }
    }
    for q in map.keys() {
        for l in arch.locations() {
            if !map.values().any(|x| *x == l) {
                let l = l.clone();
                let into_open = move |m: &QubitMap| {
                    let mut new_map = m.clone();
                    new_map.insert(*q, l);
                    return new_map;
                };
                moves.push(Box::new(into_open));
            }
        }
    }
    let rng = &mut rand::rng();
    let chosen_move = moves.choose(rng).unwrap();
    return chosen_move(&map);
}

fn sim_anneal_mapping_search<T: Architecture>(
    start: QubitMap,
    arch: &T,
    initial_temp: f64,
    term_temp: f64,
    cool_rate: f64,
    heuristic: impl Fn(&QubitMap) -> f64,
) -> QubitMap {
    return simulated_anneal(
        start,
        initial_temp,
        term_temp,
        cool_rate,
        |m| random_neighbor(m, arch),
        heuristic,
    );
}

fn route<
    A: Architecture,
    R: Transition<G, A> + Debug,
    G: GateImplementation + Debug,
    I: IntoIterator<Item = G>,
    J: IntoIterator<Item = R>,
>(
    c: &Circuit,
    arch: &A,
    map: &QubitMap,
    transitions: &impl Fn(&Step<G>) -> J,
    implement_gate: &impl Fn(&Step<G>, &A, &Gate) -> I,
    step_cost: fn(&Step<G>, &A) -> f64,
    map_eval: &impl Fn(&Circuit, &QubitMap) -> f64,
    explore_routing_orders: bool,
    crit_table: &HashMap<usize, usize>,
    id: usize,
) -> CompilerResult<G> {
    let mut steps = Vec::new();
    let mut trans_taken = Vec::new();
    let mut step_0 = Step {
        map: map.clone(),
        implemented_gates: HashSet::new(),
    };
    let mut current_circ = c.clone();
    let mut cost = step_cost(&step_0, arch);
    let executable = &c.get_front_layer();
    let mut routing_search_cool_rate = CONFIG.routing_search_cool_rate;
    let routing_search_initial_temp = CONFIG.routing_search_initial_temp;
    let routing_search_term_temp = CONFIG.routing_search_term_temp;
    if id < 4 {
        routing_search_cool_rate = CONFIG.limited_search_cool_rates[id];
    }
    if explore_routing_orders {
        step_0.max_step_all_orders(
            executable,
            arch,
            &implement_gate,
            crit_table,
            routing_search_initial_temp,
            routing_search_term_temp,
            routing_search_cool_rate,
        );
    } else {
        step_0.max_step(executable, arch, &implement_gate);
    }
    current_circ.remove_gates(&(step_0.gates()));
    steps.push(step_0);
    while current_circ.gates.len() > 0 {
        let best = find_best_next_step(
            &current_circ,
            arch,
            &transitions,
            &implement_gate,
            steps.last().unwrap(),
            step_cost,
            &map_eval,
            explore_routing_orders,
            &crit_table,
            id,
        );
        match best {
            Some((s, trans, _b)) => {
                current_circ.remove_gates(&s.gates());
                cost += step_cost(&s, arch);
                steps.push(s);
                trans_taken.push(trans.repr());
                cost += trans.cost(arch);
            }
            None => {
                panic!("No valid next step found");
            }
        }
    }
    return CompilerResult {
        steps,
        transitions: trans_taken,
        cost,
        thread_id: Some(id.to_string()),
        elapsed_time: None,
    };
}

fn find_best_next_step<
    A: Architecture,
    R: Transition<G, A> + Debug,
    G: GateImplementation,
    I: IntoIterator<Item = G>,
    J: IntoIterator<Item = R>,
>(
    c: &Circuit,
    arch: &A,
    transitions: &impl Fn(&Step<G>) -> J,
    implement_gate: impl Fn(&Step<G>, &A, &Gate) -> I,
    last_step: &Step<G>,
    step_cost: fn(&Step<G>, &A) -> f64,
    map_eval: impl Fn(&Circuit, &QubitMap) -> f64,
    explore_routing_orders: bool,
    crit_table: &HashMap<usize, usize>,
    id: usize,
) -> Option<(Step<G>, R, f64)> {
    let mut best_options = Vec::new();
    let mut best_cost = std::f64::MAX;
    let executable = c.layers().next().unwrap_or(vec![]);
    let next_layer = c.layers().next().unwrap_or(vec![]);
    let mut routing_search_cool_rate = CONFIG.routing_search_cool_rate;
    let routing_search_initial_temp = CONFIG.routing_search_initial_temp;
    let routing_search_term_temp = CONFIG.routing_search_term_temp;
    if id < 4 {
        routing_search_cool_rate = CONFIG.limited_search_cool_rates[id];
    }
    for trans in transitions(last_step) {
        let mut next_step = trans.apply(last_step);

        if explore_routing_orders {
            next_step.max_step_all_orders(
                &executable,
                arch,
                &implement_gate,
                crit_table,
                routing_search_initial_temp,
                routing_search_term_temp,
                routing_search_cool_rate,
            );
        } else {
            next_step.max_step(&executable, arch, &implement_gate);
        }
        let s_cost = step_cost(&next_step, arch);
        let t_cost = trans.cost(arch);
        let front_layer_cost =
            map_eval(&circuit_from_gates(&executable), &next_step.map) / (executable.len() as f64);
        let next_layer_cost =
            map_eval(&circuit_from_gates(&next_layer), &next_step.map) / (next_layer.len() as f64);
        let m_cost = front_layer_cost + CONFIG.extended_set_weight * next_layer_cost;
        let total_criticality: usize = next_step
            .gates()
            .into_iter()
            .map(|x| crit_table[&x.id])
            .sum();
        let weighted_vals = std::iter::zip(
            vec![CONFIG.alpha, CONFIG.beta, CONFIG.gamma, CONFIG.delta],
            vec![s_cost, t_cost, m_cost, -(total_criticality as f64)],
        );
        let cost = drop_zeros_and_normalize(weighted_vals);
        // println!(
        //     "executable : {:?}, transition : {:?} , cost : {:?}",
        //     executable, trans, cost
        // );
        if cost <= best_cost {
            if cost < best_cost {
                best_options.clear();
                best_cost = cost;
            }
            best_options.push((next_step, trans, cost));
        }
    }

    if best_options.is_empty() {
        None
    } else {
        let index = rand::random_range(..best_options.len());
        Some(best_options.remove(index))
    }
}

pub fn solve<
    A: Architecture + Send + Sync + Clone + 'static,
    R: Transition<G, A> + Debug,
    G: GateImplementation + Debug,
    I: IntoIterator<Item = G>,
    J: IntoIterator<Item = R>,
>(
    c: &Circuit,
    arch: &A,
    transitions: &impl Fn(&Step<G>) -> J,
    implement_gate: &impl Fn(&Step<G>, &A, &Gate) -> I,
    step_cost: fn(&Step<G>, &A) -> f64,
    mapping_heuristic: Option<fn(&A, &Circuit, &QubitMap) -> f64>,
    explore_routing_orders: bool,
) -> CompilerResult<G> {
    let crit_table = &build_criticality_table(c);
    match mapping_heuristic {
        Some(heuristic) => {
            let map_h = |m: &QubitMap| heuristic(arch, c, m);
            let route_h = |c: &Circuit, m: &QubitMap| heuristic(arch, c, m);
            let isom_map = incremental_isomorphism_map_with_timeout(
                c,
                arch,
                Duration::from_secs(CONFIG.isom_search_timeout),
            );

            let isom_cost = isom_map.clone().map(|x| map_h(&x));

            let sa_map = match isom_cost {
                Some(c) if c == 0.0 => None,
                _ => Some(sim_anneal_mapping_search(
                    isom_map.clone().unwrap_or_else(|| random_map(c, arch)),
                    arch,
                    CONFIG.mapping_search_initial_temp,
                    CONFIG.mapping_search_term_temp,
                    CONFIG.mapping_search_cool_rate,
                    map_h,
                )),
            };
            let sa_cost = sa_map.clone().map(|x| map_h(&x));
            let map = match (isom_cost, sa_cost) {
                (Some(i_c), None) => isom_map.unwrap(),
                (Some(i_c), Some(s_c)) if i_c < s_c => isom_map.unwrap(),
                _ => sa_map.unwrap(),
            };
            // println!("locations {:?}, map : {:?}", arch.locations(), map);
            return route(
                c,
                arch,
                &map,
                transitions,
                &implement_gate,
                step_cost,
                &route_h,
                explore_routing_orders,
                crit_table,
                0,
            );
        }
        None => {
            let map = random_map(c, arch);
            return route(
                c,
                arch,
                &map,
                transitions,
                &implement_gate,
                step_cost,
                &|_c, _m| 0.0,
                explore_routing_orders,
                crit_table,
                0,
            );
        }
    }
}

pub fn sabre_solve<
    A: Architecture + Send + Sync + Clone + 'static,
    R: Transition<G, A> + Debug,
    G: GateImplementation + Debug,
    I: IntoIterator<Item = G>,
>(
    c: &Circuit,
    arch: &A,
    transitions: &impl Fn(&Step<G>) -> Vec<R>,
    implement_gate: &impl Fn(&Step<G>, &A, &Gate) -> I,
    step_cost: fn(&Step<G>, &A) -> f64,
    mapping_heuristic: Option<fn(&A, &Circuit, &QubitMap) -> f64>,
    explore_routing_orders: bool,
) -> CompilerResult<G> {
    match serde_json::to_writer(std::fs::File::create("config_full.json").unwrap(), &*CONFIG) {
        Ok(_) => (),
        Err(e) => panic!("Error writing config file {}", e),
    }
    let crit_table = &build_criticality_table(c);
    let mut map = match mapping_heuristic {
        Some(heuristic) => {
            let map_h = |m: &QubitMap| heuristic(arch, c, m);
            let isom_map: Option<HashMap<Qubit, Location>> =
                incremental_isomorphism_map_with_timeout(
                    c,
                    arch,
                    Duration::from_secs(CONFIG.isom_search_timeout),
                );

            let isom_cost = isom_map.clone().map(|x| map_h(&x));
            let sa_map = match isom_cost {
                Some(c) if c == 0.0 => None,
                _ => Some(sim_anneal_mapping_search(
                    isom_map.clone().unwrap_or_else(|| random_map(c, arch)),
                    arch,
                    CONFIG.mapping_search_initial_temp,
                    CONFIG.mapping_search_term_temp,
                    CONFIG.mapping_search_cool_rate,
                    map_h,
                )),
            };
            let sa_cost = sa_map.clone().map(|x| map_h(&x));
            match (isom_cost, sa_cost) {
                (Some(i_c), Some(s_c)) if i_c < s_c => isom_map.unwrap(),
                _ => sa_map.unwrap(),
            }
        }
        None => random_map(c, arch),
    };
    let route_h: Box<dyn Fn(&Circuit, &QubitMap) -> f64> =
        if let Some(ref heuristic) = mapping_heuristic {
            Box::new(|c: &Circuit, m: &QubitMap| heuristic(arch, c, m))
        } else {
            Box::new(|_c: &Circuit, _m: &QubitMap| 0.0)
        };

    for _ in 0..CONFIG.sabre_iterations {
        for circ in [c, &c.reversed()] {
            let res = route(
                circ,
                arch,
                &map,
                transitions,
                &implement_gate,
                step_cost,
                &route_h,
                explore_routing_orders,
                crit_table,
                0,
            );
            map = res.steps.last().unwrap().map.clone();
        }
    }
    return route(
        c,
        arch,
        &map,
        transitions,
        &implement_gate,
        step_cost,
        &route_h,
        explore_routing_orders,
        crit_table,
        0,
    );
}

pub fn solve_with_cached_heuristic<
    A: Architecture + Send + Sync + Clone + 'static,
    R: Transition<G, A> + Debug,
    G: GateImplementation + Debug,
    I: IntoIterator<Item = G>,
>(
    c: &Circuit,
    arch: &A,
    transitions: &impl Fn(&Step<G>) -> Vec<R>,
    implement_gate: impl Fn(&Step<G>, &A, &Gate) -> I,
    step_cost: fn(&Step<G>, &A) -> f64,
    mapping_heuristic: Option<fn(&A, &Circuit, &QubitMap) -> f64>,
    delta_on_move: impl Fn(&QubitMap, Move) -> f64,
    explore_routing_orders: bool,
) -> CompilerResult<G> {
    let crit_table = &build_criticality_table(c);
    let mut map = match mapping_heuristic {
        Some(heuristic) => {
            let map_h = |m: &QubitMap| heuristic(arch, c, m);
            let isom_map: Option<HashMap<Qubit, Location>> =
                incremental_isomorphism_map_with_timeout(
                    c,
                    arch,
                    Duration::from_secs(CONFIG.isom_search_timeout),
                );

            let isom_cost = isom_map.clone().map(|x| map_h(&x));
            let sa_map = match isom_cost {
                Some(c) if c == 0.0 => None,
                _ => Some(fast_mapping_simulated_anneal(
                    &isom_map.clone().unwrap_or_else(|| random_map(c, arch)),
                    arch,
                    CONFIG.mapping_search_initial_temp,
                    CONFIG.mapping_search_term_temp,
                    CONFIG.mapping_search_cool_rate,
                    map_h,
                    delta_on_move,
                )),
            };

            let sa_cost = sa_map.clone().map(|x| map_h(&x));
            match (isom_cost, sa_cost) {
                (Some(i_c), Some(s_c)) if i_c < s_c => isom_map.unwrap(),
                _ => sa_map.unwrap(),
            }
        }
        None => random_map(c, arch),
    };
    let route_h: Box<dyn Fn(&Circuit, &QubitMap) -> f64> =
        if let Some(ref heuristic) = mapping_heuristic {
            Box::new(|c: &Circuit, m: &QubitMap| heuristic(arch, c, m))
        } else {
            Box::new(|_c: &Circuit, _m: &QubitMap| 0.0)
        };

    for _ in 0..CONFIG.sabre_iterations {
        for circ in [c, &c.reversed()] {
            let res = route(
                circ,
                arch,
                &map,
                transitions,
                &implement_gate,
                step_cost,
                &route_h,
                explore_routing_orders,
                crit_table,
                0,
            );
            map = res.steps.last().unwrap().map.clone();
        }
    }
    return route(
        c,
        arch,
        &map,
        transitions,
        &implement_gate,
        step_cost,
        &route_h,
        explore_routing_orders,
        crit_table,
        0,
    );
}

pub fn solve_parallel<
    A: Architecture + Send + Sync + Clone + 'static,
    R: Transition<G, A> + Debug,
    G: GateImplementation + Debug + Send,
    I: IntoIterator<Item = G>,
>(
    c: &Circuit,
    arch: &A,
    transitions: &(impl Fn(&Step<G>) -> Vec<R> + std::marker::Sync),
    implement_gate: impl Fn(&Step<G>, &A, &Gate) -> I + std::marker::Sync + std::marker::Send,
    step_cost: fn(&Step<G>, &A) -> f64,
    mapping_heuristic: Option<fn(&A, &Circuit, &QubitMap) -> f64>,
    explore_routing_orders: bool,
) -> CompilerResult<G> {
    (0..CONFIG.parallel_searches)
        .into_par_iter()
        .map(|_| {
            solve(
                c,
                arch,
                transitions,
                &implement_gate,
                step_cost,
                mapping_heuristic,
                explore_routing_orders,
            )
        })
        .min_by(|a, b| {
            // if cost is f64, handle NaN/partial_cmp
            a.cost
                .partial_cmp(&b.cost)
                .unwrap_or(std::cmp::Ordering::Equal)
        })
        .expect("num_trials should be > 0")
}

pub fn sabre_solve_parallel<
    A: Architecture + Send + Sync + Clone + 'static,
    R: Transition<G, A> + Debug,
    G: GateImplementation + Debug + Send,
    I: IntoIterator<Item = G>,
>(
    c: &Circuit,
    arch: &A,
    transitions: &(impl Fn(&Step<G>) -> Vec<R> + std::marker::Sync),
    implement_gate: impl Fn(&Step<G>, &A, &Gate) -> I + std::marker::Sync + std::marker::Send,
    step_cost: fn(&Step<G>, &A) -> f64,
    mapping_heuristic: Option<fn(&A, &Circuit, &QubitMap) -> f64>,
    explore_routing_orders: bool,
) -> CompilerResult<G> {
    (0..CONFIG.parallel_searches)
        .into_par_iter()
        .map(|_| {
            sabre_solve(
                c,
                arch,
                transitions,
                &implement_gate,
                step_cost,
                mapping_heuristic,
                explore_routing_orders,
            )
        })
        .min_by(|a, b| {
            // if cost is f64, handle NaN/partial_cmp
            a.cost
                .partial_cmp(&b.cost)
                .unwrap_or(std::cmp::Ordering::Equal)
        })
        .expect("num_trials should be > 0")
}

pub fn solve_joint_optimize<
    A: Architecture + Send + Sync + Clone + 'static,
    R: Transition<G, A> + Debug,
    G: GateImplementation + Debug,
    I: IntoIterator<Item = G>,
    J: IntoIterator<Item = R>,
>(
    c: &Circuit,
    arch: &A,
    transitions: &impl Fn(&Step<G>) -> J,
    implement_gate: impl Fn(&Step<G>, &A, &Gate) -> I,
    step_cost: fn(&Step<G>, &A) -> f64,
    mapping_heuristic: Option<fn(&A, &Circuit, &QubitMap) -> f64>,
    explore_routing_orders: bool,
    id: usize,
) -> CompilerResult<G> {
    let start = Instant::now();
    // register SIGINT/SIGTERM handler
    let terminate = Arc::new(AtomicBool::new(false));
    flag::register(SIGINT, Arc::clone(&terminate)).expect("Failed to register SIGINT handler");
    flag::register(SIGTERM, Arc::clone(&terminate)).expect("Failed to register SIGTERM handler");

    let isom_map: Option<HashMap<Qubit, Location>> = incremental_isomorphism_map_with_timeout(
        c,
        arch,
        Duration::from_secs(CONFIG.isom_search_timeout),
    );
    let start_map = isom_map.unwrap_or_else(|| random_map(c, arch));
    let crit_table = &build_criticality_table(c);
    let route_h: Box<dyn Fn(&Circuit, &QubitMap) -> f64> =
        if let Some(ref heuristic) = mapping_heuristic {
            Box::new(move |c, m| heuristic(arch, c, m))
        } else {
            Box::new(|_, _| 0.0)
        };

    // initial solution
    let mut best_res = route(
        c,
        arch,
        &start_map,
        transitions,
        &implement_gate,
        step_cost,
        &route_h,
        explore_routing_orders,
        crit_table,
        id,
    );
    let mut best_cost = best_res.cost;
    let mut current_map = start_map;
    let mut current_cost = best_cost;
    let mut temp = CONFIG.mapping_search_initial_temp;
    let current_time = Instant::now();
    let elapsed = Duration::as_secs(&(current_time - start));
    let to_write = CompilerResult {
        elapsed_time: Some(elapsed),
        ..best_res
    };
    {
        let stdout = std::io::stdout();
        let mut out = stdout.lock();
        let _ = serde_json::to_writer(&mut out, &to_write).map_err(IOError::OutputErr);
        let _ = out.write_all(b"\n");
    }

    // simulated annealing loop
    while temp > CONFIG.mapping_search_term_temp {
        // check for SIGINT/SIGTERM
        if terminate.load(Ordering::Relaxed) {
            eprintln!(
                "Termination signal received in thread {}— returning best solution so far (cost = {})",
                id,
                best_cost
            );
            break;
        }

        let next = random_neighbor(&current_map, arch);
        let next_res = route(
            c,
            arch,
            &next,
            transitions,
            &implement_gate,
            step_cost,
            &route_h,
            explore_routing_orders,
            crit_table,
            id,
        );
        let next_cost = next_res.cost;

        let delta_curr = next_cost - current_cost;
        let delta_best = next_cost - best_cost;
        let accept = rand::random::<f64>() < (-delta_curr / temp).exp();

        if delta_best < 0.0 {
            best_res = next_res;
            best_cost = next_cost;
            current_map = next;
            current_cost = next_cost;
            let current_time = Instant::now();
            let elapsed = Duration::as_secs(&(current_time - start));
            let to_write = CompilerResult {
                elapsed_time: Some(elapsed),
                ..best_res
            };
            {
                let stdout = std::io::stdout();
                let mut out = stdout.lock();
                let _ = serde_json::to_writer(&mut out, &to_write).map_err(IOError::OutputErr);
                let _ = out.write_all(b"\n");
            }
        } else if accept {
            current_map = next;
            current_cost = next_cost;
        }

        temp *= CONFIG.mapping_search_cool_rate;
    }

    to_write
}

pub fn solve_joint_optimize_parallel<
    A: Architecture + Send + Sync + Clone + 'static,
    R: Transition<G, A> + Debug,
    G: GateImplementation + Debug + Send,
    I: IntoIterator<Item = G>,
    J: IntoIterator<Item = R>,
>(
    c: &Circuit,
    arch: &A,
    transitions: &(impl Fn(&Step<G>) -> J + std::marker::Sync),
    implement_gate: impl Fn(&Step<G>, &A, &Gate) -> I + std::marker::Sync + std::marker::Send,
    step_cost: fn(&Step<G>, &A) -> f64,
    mapping_heuristic: Option<fn(&A, &Circuit, &QubitMap) -> f64>,
    explore_routing_orders: bool,
) -> CompilerResult<G> {
    (0..CONFIG.parallel_searches)
        .into_par_iter()
        .enumerate()
        .map(|(id, _)| {
            solve_joint_optimize(
                c,
                arch,
                transitions,
                &implement_gate,
                step_cost,
                mapping_heuristic,
                explore_routing_orders,
                id,
            )
        })
        .min_by(|a, b| {
            // if cost is f64, handle NaN/partial_cmp
            a.cost
                .partial_cmp(&b.cost)
                .unwrap_or(std::cmp::Ordering::Equal)
        })
        .expect("num_trials should be > 0")
}

use crate::config::CONFIG;
use crate::utils::simulated_anneal;
use crate::utils::swap_random_array_elements;
use itertools::Itertools;
use petgraph::graph::NodeIndex;
use petgraph::Graph;
use serde::Deserialize;
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Add;
use std::ops::Div;
use std::ops::Index;
use std::ops::Mul;
use std::ops::Sub;

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug, Serialize)]
pub struct Qubit(usize);
impl Qubit {
    pub fn new(i: usize) -> Self {
        return Qubit(i);
    }
    pub fn get_index(&self) -> usize {
        return self.0;
    }
}

impl<T> Index<Qubit> for Vec<T> {
    type Output = T;

    fn index(&self, q : Qubit) -> &Self::Output {
        &self[q.get_index()]
    }
}


#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug, Default, Serialize, Deserialize)]
pub struct Location(usize);

impl<T> Index<Location> for Vec<T> {
    type Output = T;

    fn index(&self, loc: Location) -> &Self::Output {
        &self[loc.get_index()]
    }
}

// Implement +
impl Add for Location {
    type Output = Location;

    fn add(self, rhs: Location) -> Location {
        Location(self.0 + rhs.0)
    }
}

// Implement -
impl Sub for Location {
    type Output = Location;

    fn sub(self, rhs: Location) -> Location {
        Location(self.0 - rhs.0)
    }
}

// Implement *
impl Mul for Location {
    type Output = Location;

    fn mul(self, rhs: Location) -> Location {
        Location(self.0 * rhs.0)
    }
}

// Implement /
impl Div for Location {
    type Output = Location;

    fn div(self, rhs: Location) -> Location {
        Location(self.0 / rhs.0)
    }
}

impl Add<usize> for Location {
    type Output = Location;
    fn add(self, rhs: usize) -> Location {
        Location(self.0 + rhs)
    }
}

impl Sub<usize> for Location {
    type Output = Location;
    fn sub(self, rhs: usize) -> Location {
        Location(self.0 - rhs)
    }
}

impl Mul<usize> for Location {
    type Output = Location;
    fn mul(self, rhs: usize) -> Location {
        Location(self.0 * rhs)
    }
}

impl Div<usize> for Location {
    type Output = Location;
    fn div(self, rhs: usize) -> Location {
        Location(self.0 / rhs)
    }
}

pub type QubitMap = HashMap<Qubit, Location>;

impl Location {
    pub fn new(i: usize) -> Self {
        return Location(i);
    }
    pub fn get_index(&self) -> usize {
        return self.0;
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub enum PauliTerm {
    PauliI,
    PauliX,
    PauliY,
    PauliZ,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub enum Operation {
    CX,
    T,
    PauliRot {
        axis: Vec<PauliTerm>,
        angle: (isize, usize),
    },
    PauliMeasurement {
        sign: bool,
        axis: Vec<PauliTerm>,
    },
}
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub enum GateType {
    CX,
    T,
    PauliRot,
    PauliMeasurement,
}

#[derive(Clone, Debug, Eq, Hash, Serialize)]
pub struct Gate {
    pub operation: Operation,
    pub qubits: Vec<Qubit>,
    pub id: usize,
}

impl Gate {
    fn filter_by_pauli_term(&self, term: &PauliTerm) -> Vec<Qubit> {
        match &self.operation {
            Operation::CX | Operation::T => vec![],
            Operation::PauliRot { axis, .. } | Operation::PauliMeasurement { axis, .. } => (0
                ..axis.len())
                .filter(|i| axis[*i] == *term)
                .map(Qubit::new)
                .collect(),
        }
    }

    pub fn x_indices(&self) -> Vec<Qubit> {
        self.filter_by_pauli_term(&PauliTerm::PauliX)
    }

    pub fn y_indices(&self) -> Vec<Qubit> {
        self.filter_by_pauli_term(&PauliTerm::PauliY)
    }

    pub fn z_indices(&self) -> Vec<Qubit> {
        self.filter_by_pauli_term(&PauliTerm::PauliZ)
    }

    pub fn gate_type(&self) -> GateType {
        match &self.operation {
            Operation::CX => GateType::CX,
            Operation::T => GateType::T,
            Operation::PauliRot { axis, angle } => GateType::PauliRot,
            Operation::PauliMeasurement { sign, axis } => GateType::PauliMeasurement,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Circuit {
    pub gates: Vec<Gate>,
    pub qubits: HashSet<Qubit>,
}

impl PartialEq for Gate {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Circuit {
    pub fn layers(&self) -> Layers {
        Layers {
            remaining: self.gates.clone(),
        }
    }

    pub fn get_front_layer(&self) -> Vec<Gate> {
        let mut blocked_qubits: HashSet<Qubit> = HashSet::new();
        let mut gates = Vec::new();
        for g in &self.gates {
            let gate_qubits = &g.qubits;
            let not_blocked = gate_qubits.iter().all(|q| !blocked_qubits.contains(q));
            if not_blocked {
                gates.push(g.clone());
            }
            blocked_qubits.extend(gate_qubits);
        }
        return gates;
    }
    pub fn remove_gates(&mut self, gates: &Vec<Gate>) {
        self.gates.retain(|g| !gates.contains(g));
    }
    pub fn reversed(&self) -> Circuit {
        let mut copy = self.clone();
        copy.gates.reverse();
        return copy;
    }
}

pub struct Layers {
    remaining: Vec<Gate>,
}

impl Iterator for Layers {
    type Item = Vec<Gate>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining.is_empty() {
            return None;
        }

        let mut blocked_qubits: HashSet<Qubit> = HashSet::new();
        let mut this_layer = Vec::new();
        let mut rest = Vec::new();

        // drain all remaining, partition into layer vs. rest
        for gate in self.remaining.drain(..) {
            let gate_qubits = &gate.qubits;
            let not_blocked = gate_qubits.iter().all(|q| !blocked_qubits.contains(q));
            if not_blocked {
                // none of its qubits are blocked → include in this layer
                this_layer.push(gate.clone());
            } else {
                // had a conflict → defer to next iteration
                rest.push(gate.clone());
            }
            blocked_qubits.extend(gate_qubits);
        }

        // keep the leftovers for the next round
        self.remaining = rest;
        Some(this_layer)
    }
}

pub fn circuit_from_gates(gates: &[Gate]) -> Circuit {
    let mut qubits = HashSet::new();
    for gate in gates {
        for qubit in &gate.qubits {
            qubits.insert(*qubit);
        }
    }
    return Circuit {
        gates: gates.to_vec(),
        qubits,
    };
}

pub trait GateImplementation: Clone + Serialize + Hash + Eq + Debug {}

#[derive(Clone, Debug, Serialize)]
pub struct Step<T: GateImplementation> {
    pub map: QubitMap,
    pub implemented_gates: HashSet<ImplementedGate<T>>,
}

impl<G: GateImplementation> Step<G> {
    pub fn max_step<A: Architecture, I: IntoIterator<Item = G>>(
        &mut self,
        executable: &Vec<Gate>,
        arch: &A,
        implement_gate: &impl Fn(&Step<G>, &A, &Gate) -> I,
    ) {
        assert!(self.implemented_gates.is_empty());
        for gate in executable {
            let implementation = implement_gate(self, arch, gate).into_iter().next();
            match implementation {
                None => continue,
                Some(implementation) => {
                    self.implemented_gates.insert(ImplementedGate {
                        gate: gate.clone(),
                        implementation,
                    });
                }
            }
        }
    }

    pub fn max_step_all_orders<A: Architecture, I: IntoIterator<Item = G>>(
        &mut self,
        executable: &Vec<Gate>,
        arch: &A,
        implement_gate: impl Fn(&Step<G>, &A, &Gate) -> I,
        crit_table: &HashMap<usize, usize>,
        routing_search_initial_temp: f64,
        routing_search_term_temp: f64,
        routing_search_cool_rate: f64,
    ) {
        assert!(self.implemented_gates.is_empty());
        let mut best_total_criticality = 0;
        let orders = executable.iter().cloned().permutations(executable.len());
        if executable.len() < CONFIG.exhaustive_search_threshold {
            for order in orders {
                let mut step = Step {
                    map: self.map.clone(),
                    implemented_gates: HashSet::new(),
                };
                step.max_step(&order, arch, &implement_gate);
                let candidate_total_criticality: usize =
                    step.gates().into_iter().map(|x| crit_table[&x.id]).sum();

                if candidate_total_criticality > best_total_criticality {
                    *self = step;
                    best_total_criticality = candidate_total_criticality;
                }
                if self.implemented_gates.len() == executable.len() {
                    return;
                }
            }
        } else {
            let cost_function = |order: &Vec<Gate>| {
                let mut step = Step {
                    map: self.map.clone(),
                    implemented_gates: HashSet::new(),
                };
                step.max_step(&order, arch, &implement_gate);
                return step
                    .gates()
                    .into_iter()
                    .map(|x| crit_table[&x.id])
                    .sum::<usize>() as f64;
            };
            let random_neighbor = swap_random_array_elements;
            let best_order = simulated_anneal(
                executable.clone(),
                routing_search_initial_temp,
                routing_search_term_temp,
                routing_search_cool_rate,
                random_neighbor,
                cost_function,
            );
            let mut step = Step {
                map: self.map.clone(),
                implemented_gates: HashSet::new(),
            };
            step.max_step(&best_order, arch, &implement_gate);
            *self = step;
        }
    }

    pub fn max_step_all_implementations<A: Architecture, I: IntoIterator<Item = G>>(
        &mut self,
        executable: &Vec<Gate>,
        arch: &A,
        implement_gate: impl Fn(&Step<G>, &A, &Gate) -> I,
    ) {
        assert!(self.implemented_gates.is_empty());
        let orders = executable.iter().cloned().permutations(executable.len());
        for order in orders {
            for gate in order {
                let mut seen = HashSet::new();
                for implementation in implement_gate(self, arch, &gate).into_iter() {
                    let seen = seen.insert(implementation.clone());
                    assert!(seen);
                    if !self.gates().contains(&gate) {
                        self.implemented_gates.insert(ImplementedGate {
                            gate: gate.clone(),
                            implementation,
                        });
                    }
                }
            }
        }
    }
    pub fn gates(&self) -> Vec<Gate> {
        return self
            .implemented_gates
            .iter()
            .map(|gi| gi.gate.clone())
            .collect();
    }

    pub fn map(&self) -> &QubitMap {
        return &self.map;
    }

    pub fn implemented_gates(&self) -> HashSet<ImplementedGate<G>> {
        return self.implemented_gates.clone();
    }
}

pub trait Transition<T: GateImplementation, A: Architecture> {
    fn apply(&self, step: &Step<T>) -> Step<T>;
    fn repr(&self) -> String;
    fn cost(&self, arch: &A) -> f64;
}

pub trait Architecture {
    fn locations(&self) -> Vec<Location>;
    fn graph(&self) -> (Graph<Location, ()>, HashMap<Location, NodeIndex>);
}

#[derive(Debug, Serialize, Clone, Hash, PartialEq, Eq)]
pub struct ImplementedGate<T: GateImplementation> {
    pub gate: Gate,
    pub implementation: T,
}

#[derive(Debug, Serialize)]
pub struct CompilerResult<T: GateImplementation> {
    pub steps: Vec<Step<T>>,
    pub transitions: Vec<String>,
    pub cost: f64,
    pub thread_id : Option<String>,
    pub elapsed_time : Option<u64>
}

#[cfg(test)]
mod tests {
    use super::*;
    use petgraph::Graph;

    // ── Minimal mock types ────────────────────────────────────────────────────

    #[derive(Clone, Debug, Serialize, Hash, PartialEq, Eq)]
    struct MockImpl {
        remote: bool,
    }
    impl MockImpl {
        fn remote(&self) -> bool { self.remote }
    }
    impl GateImplementation for MockImpl {}

    struct MockArch {
        n_comm_qubits: usize,
        bell_success_prob: f64,
        bell_attempt_interval: f64,
        max_bell_rate: f64,
        code_distance: usize,
    }
    impl MockArch {
        fn bell_pair_rate(&self) -> f64 {
            if self.bell_attempt_interval == 0.0 { return f64::MAX; }
            let rate_from_qubits = self.n_comm_qubits as f64 / self.bell_attempt_interval;
            self.bell_success_prob * rate_from_qubits.min(self.max_bell_rate)
        }
        fn syndrome_bell_demand(&self) -> usize { 2 * self.code_distance }
        fn max_bell_pairs_per_cycle(&self, cycle_time: f64) -> usize {
            let rate = self.bell_pair_rate();
            if rate >= f64::MAX / 2.0 { return usize::MAX; }
            (rate * cycle_time) as usize
        }
        fn gate_bell_budget(&self, cycle_time: f64) -> usize {
            self.max_bell_pairs_per_cycle(cycle_time)
                .saturating_sub(self.syndrome_bell_demand())
        }
        fn is_saturated(&self) -> bool {
            if self.bell_attempt_interval == 0.0 { return false; }
            (self.n_comm_qubits as f64 / self.bell_attempt_interval) >= self.max_bell_rate
        }
    }
    impl Architecture for MockArch {
        fn locations(&self) -> Vec<Location> { vec![] }
        fn graph(&self) -> (Graph<Location, ()>, HashMap<Location, NodeIndex>) {
            (Graph::new(), HashMap::new())
        }
    }

    fn sinclair_arch() -> MockArch {
        MockArch {
            n_comm_qubits: 160,
            bell_success_prob: 0.10,
            bell_attempt_interval: 1.0,
            max_bell_rate: 10.0,
            code_distance: 20,
        }
    }

    fn cx_gate(id: usize) -> Gate {
        Gate { operation: Operation::CX, qubits: vec![Qubit::new(0), Qubit::new(1)], id }
    }

    fn empty_step() -> Step<MockImpl> {
        Step { map: HashMap::new(), implemented_gates: HashSet::new() }
    }

    // ── Bell pair hardware math (Sinclair et al. numbers) ────────────────────

    #[test]
    fn test_bell_pair_rate_sinclair_params() {
        let arch = sinclair_arch();
        // R_Bell = P_aa × min(N_comm/τ, R_max) = 0.10 × min(160, 10) = 1.0 MHz
        assert!((arch.bell_pair_rate() - 1.0).abs() < 1e-9);
    }

    #[test]
    fn test_is_saturated_sinclair_params() {
        let arch = sinclair_arch();
        // 160/1.0 = 160 >= R_max=10 → saturated
        assert!(arch.is_saturated());
    }

    #[test]
    fn test_syndrome_bell_demand_sinclair_params() {
        let arch = sinclair_arch();
        // 2 × code_distance = 2 × 20 = 40
        assert_eq!(arch.syndrome_bell_demand(), 40);
    }

    #[test]
    fn test_gate_bell_budget_zero_at_baseline() {
        let arch = sinclair_arch();
        // t_cycle = syndrome_demand / R_Bell = 40 / 1.0 = 40 μs
        // max_bell_pairs = floor(1.0 × 40) = 40
        // budget = 40 - 40 = 0
        let t_cycle = arch.syndrome_bell_demand() as f64 / arch.bell_pair_rate();
        assert_eq!(arch.gate_bell_budget(t_cycle), 0);
    }

    // ── Budget deferral via max_step ─────────────────────────────────────────

    /// Simulates the emitted budget wrapper: with budget=0, all remote gates
    /// should be deferred (max_step implements zero gates).
    #[test]
    fn test_budget_zero_defers_all_remote_gates() {
        let arch = sinclair_arch();
        let gates = vec![cx_gate(0), cx_gate(1), cx_gate(2)];
        let mut step = empty_step();

        let budgeted = |step: &Step<MockImpl>, arch: &MockArch, _gate: &Gate| -> Option<MockImpl> {
            let bell_rate = arch.bell_pair_rate();
            let t_cycle = arch.syndrome_bell_demand() as f64 / bell_rate;
            let budget = arch.gate_bell_budget(t_cycle);
            let remote_count = step.implemented_gates.iter()
                .filter(|g| g.implementation.remote())
                .count();
            if remote_count >= budget { return None; }
            Some(MockImpl { remote: true })
        };

        step.max_step(&gates, &arch, &budgeted);
        // budget=0 → every gate should be deferred
        assert_eq!(step.implemented_gates.len(), 0,
            "Expected all remote gates deferred with budget=0, but {} were implemented",
            step.implemented_gates.len());
    }

    /// With budget=1, exactly one remote gate is scheduled per cycle.
    #[test]
    fn test_budget_one_allows_single_remote_gate() {
        // Increase t_cycle so budget = floor(1.0 × 41) - 40 = 1
        let arch = sinclair_arch();
        let gates = vec![cx_gate(0), cx_gate(1), cx_gate(2)];
        let mut step = empty_step();

        let budgeted = |step: &Step<MockImpl>, arch: &MockArch, _gate: &Gate| -> Option<MockImpl> {
            let bell_rate = arch.bell_pair_rate();
            // Use t_cycle = 41 μs → budget = floor(1.0×41) - 40 = 1
            let budget = arch.gate_bell_budget(41.0);
            let remote_count = step.implemented_gates.iter()
                .filter(|g| g.implementation.remote())
                .count();
            if remote_count >= budget { return None; }
            Some(MockImpl { remote: true })
        };

        step.max_step(&gates, &arch, &budgeted);
        assert_eq!(step.implemented_gates.len(), 1,
            "Expected exactly 1 remote gate with budget=1, got {}",
            step.implemented_gates.len());
    }

    /// Local gates (remote=false) are never counted against the Bell pair budget.
    #[test]
    fn test_local_gates_not_counted_against_budget() {
        let arch = sinclair_arch();
        let gates = vec![cx_gate(0), cx_gate(1), cx_gate(2)];
        let mut step = empty_step();

        // realize_gate always returns a local gate
        let local_realize = |_step: &Step<MockImpl>, _arch: &MockArch, _gate: &Gate| -> Option<MockImpl> {
            Some(MockImpl { remote: false })
        };

        step.max_step(&gates, &arch, &local_realize);
        // All 3 local gates should be scheduled regardless of Bell pair budget
        assert_eq!(step.implemented_gates.len(), 3,
            "Local gates should not be limited by Bell pair budget");
    }
}

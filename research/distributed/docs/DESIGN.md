# Distributed Quantum Routing in Amaro - Design Document

**Problem Statement:** Extending Amaro with Multi-QPU Architecture and Inter-QPU Transition Costs

---

## 1. Motivation

Amaro currently models a single QPU as a labeled graph and generates a routing compiler via the MaxState solver. All existing case studies (nisq, scmr, ilq, mqlss, tiqmr, raa) are single-chip.

Near-term quantum hardware is moving toward multi-chip designs:
- IBM Kookaburra (planned): 1386 qubits across 3 chips (Survey.pdf Section 2)
- Distributed quantum computing requires gates between qubits on different QPUs, executed via entanglement-mediated operations (TeleGate or TeleData)

This document designs the minimal extension to Amaro that can express a two-QPU distributed routing problem while preserving MaxState's correctness guarantee (Amaro Theorem 5.7).

---

## 2. Background: How Remote Gates Work

Two paradigms for executing a gate whose qubits live on different QPUs (Survey.pdf Section 5.3):

**TeleGate** - Execute the gate remotely using a pre-shared EPR pair:
- Requires 1 communication qubit per QPU, 1 EPR pair, 2 classical bits
- Best when one control qubit drives many consecutive remote gates (amortizes EPR cost via cat-entanglement)

**TeleData** - Teleport one qubit's state to the other QPU, execute locally, teleport back:
- Requires 1 communication qubit per QPU, 1 EPR pair per direction
- Better for sparse/random remote gate patterns

Both consume exactly 1 EPR pair ("e-bit") as the fundamental communication resource.

---

## 3. Current Amaro Limitations

The `Arch` variable in Amaro is a **single** labeled graph. The following are absent:
- No way to represent multiple QPUs
- No way to express which locations belong to which QPU
- No cost concept for e-bits or entanglement link capacity
- No TeleGate / TeleData primitive in `realize_gate`
- No communication qubit distinction (data vs. comm qubits within each QPU)

**Extension points** (from Amaro Section 4.2 and Section 6 case studies):
1. `ArchInfo` `data` field - already used in nisq-ve for `succ_rates`, in scmr for `magic_state_qubits`. Adding QPU-indexed fields here is the minimal-change path.
2. `realize_gate` - already branches on gate type (CX vs T in scmr). Can branch on whether qubits are co-located vs. cross-QPU.
3. `cost` in `TransitionInfo` / `StateInfo` - already supports continuous cost functions (nisq-ve). E-bit cost maps directly here.

---

## 4. Proposed Language Extension

### 4.1 ArchInfo: Multi-QPU Fields

```
ArchInfo:
    Arch{
        num_qpus    : Int,
        qpu_sizes   : Vec<Int>,            -- number of locations per QPU
        link_cost   : Float,               -- e-bit cost per inter-QPU gate
        comm_qubits : Vec<Location>,       -- communication qubits (one per QPU)
        alg_qubits  : Vec<Location>        -- algorithmic (data) qubits
    }
    get_locations = Arch.alg_qubits()
```

**New built-in function needed:** `qpu_id(loc : Location) -> Int`
- Returns which QPU a given location belongs to
- Runtime implementation: `loc.index / (total_locations / num_qpus)` for uniform partitions, or lookup via `qpu_sizes` for heterogeneous QPUs

### 4.2 RouteInfo: Remote Gate Realization

```
RouteInfo:
    routed_gates = CX
    GateRealization{u : Location, v : Location, remote : Bool}
    realize_gate =
        if qpu_id(State.map[Gate.qubits[0]]) == qpu_id(State.map[Gate.qubits[1]])
            then
                -- Local gate: same QPU, standard connectivity check
                if Arch.contains_edge((State.map[Gate.qubits[0]], State.map[Gate.qubits[1]]))
                    then Some(GateRealization{
                            u      = State.map[Gate.qubits[0]],
                            v      = State.map[Gate.qubits[1]],
                            remote = false
                         })
                    else None
            else
                -- Remote gate: different QPUs, TeleGate via comm qubits
                Some(GateRealization{
                    u      = State.map[Gate.qubits[0]],
                    v      = State.map[Gate.qubits[1]],
                    remote = true
                })
```

**Key insight:** `qpu_id` is the single new built-in that unlocks cross-QPU routing. All other structure (conditional realize_gate, struct fields) already exists in Amaro.

### 4.3 TransitionInfo: Local Swaps Only

```
TransitionInfo:
    Transition{edge : (Location, Location)}
    get_transitions = (map(|x| -> Transition{edge = x}, Arch.edges()))
                        .push(Transition{edge = (Location(0), Location(0))})
    apply = value_swap(Transition.edge.(0), Transition.edge.(1))
    cost  = if (Transition.edge) == (Location(0), Location(0))
                then 0.0
                else 1.0
```

Note: Transitions are **intra-QPU only** (no cross-QPU SWAP). Cross-QPU "movement" is modeled as TeleData in `realize_gate`, not as a transition.

### 4.4 StateInfo: E-bit Cost

```
StateInfo:
    cost = fold(0.0,
                |x, acc| -> acc + x,
                map(|x| -> if x.implementation.(remote())
                               then Arch.link_cost
                               else 0.0,
                    State.gates()))
```

This charges `link_cost` e-bits for each remote gate realization in the current state, on top of the transition swap cost.

---

## 5. Architecture JSON Schema Extension

Current schema: `{"graph": [[u, v], ...]}` (edge list)

Proposed distributed schema:
```json
{
    "graph": [[u, v], ...],
    "num_qpus": 2,
    "qpu_sizes": [5, 5],
    "link_cost": 1.0,
    "comm_qubits": [4, 9],
    "alg_qubits": [0, 1, 2, 3, 5, 6, 7, 8]
}
```

Parser changes needed:
- `structures.rs`: Add `num_qpus: usize`, `qpu_sizes: Vec<usize>`, `link_cost: f64`, `comm_qubits: Vec<Location>` to `CustomArch`
- `solver/src/main.rs` (or arch loader): Deserialize new fields from JSON
- Generator: Emit `qpu_id(loc)` as a runtime function using `qpu_sizes` lookup

---

## 6. New Built-in Functions Required

| Function | Signature | Description |
|----------|-----------|-------------|
| `qpu_id` | `Location -> Int` | QPU index of a location |
| `same_qpu` | `(Location, Location) -> Bool` | True if both on same QPU |
| `comm_qubit_for` | `Int -> Location` | Communication qubit for QPU i |

These extend the built-in function table in `generator/build/parse.rs` and `amaro-vscode/amaro-lsp/src/parser/symbols.rs`.

---

## 7. Correctness Considerations

**Theorem 5.7 (Amaro):** MaxState's maximal-state construction is correct when the `Real` predicate is non-interfering (Definition 5.5 - two realizations don't conflict if their qubit sets are disjoint).

**For distributed routing:**
- Local gates: Non-interference is unchanged (same condition as nisq).
- Remote gates: Two remote gates `(a→b)` and `(c→d)` where `qpu_id(a) ≠ qpu_id(b)` and `qpu_id(c) ≠ qpu_id(d)` are non-interfering iff `{a, b} ∩ {c, d} = ∅` AND they don't share communication qubits.
- **Communication qubit constraint:** At most one remote gate per QPU per time step (one comm qubit per QPU in the simple model). This requires modifying the `Real` predicate to enforce comm qubit exclusivity - a new non-interference condition not present in current Amaro.

**Open question:** Can comm qubit exclusivity be expressed within the existing `GateRealization` non-interference check, or does it require a new language construct (e.g., a `resource_conflict` predicate)?

---

## 8. Implementation Roadmap

### Phase 1: Language Design (2-4 weeks)
- [ ] Finalize `qpu_id` semantics for heterogeneous QPU sizes
- [ ] Decide TeleGate vs TeleData as the default remote gate model
- [ ] Determine communication qubit capacity constraint mechanism
- [ ] Write concrete `simple_2qpu.qmrl` that compiles (even if trivially)

### Phase 2: Parser & Type System (4-6 weeks)
- [ ] Add `qpu_id`, `same_qpu` to `parse.rs` built-in table
- [ ] Add new `ArchInfo` fields to Amaro type checker
- [ ] Add `remote : Bool` field support to `GateRealization` struct
- [ ] Add `qpu_id`, `same_qpu` to extension LSP `symbols.rs`

### Phase 3: Runtime & Emitter (4-6 weeks)
- [ ] Add `num_qpus`, `qpu_sizes`, `link_cost`, `comm_qubits` to `CustomArch` in `structures.rs`
- [ ] Implement `qpu_id(loc)` in Rust runtime
- [ ] Update arch JSON loader to read new fields
- [ ] Emit `remote` field in `GateRealization` struct
- [ ] Enforce comm qubit exclusivity in MaxState's non-interference check

### Phase 4: Evaluation (3-4 weeks)
- [ ] Create 2-QPU test architecture JSON (e.g., two 5-qubit linear chains + 1 inter-QPU link)
- [ ] Compile QFT-8 circuit, measure e-bit count
- [ ] Compare against Pytket-DQC / disqco on same circuit+arch
- [ ] Verify correctness via Qiskit simulation of emitted circuit

---

## 9. Related Files

- `research/distributed/examples/simple_2qpu.qmrl` - Prototype QMRL for 2-QPU routing
- `research/distributed/examples/arch_2qpu.json` - Prototype 2-QPU architecture
- `problem-descriptions/nisq.qmrl` - Closest existing solver (basis for extension)
- `problem-descriptions/nisq_variable_err.qmrl` - Variable cost model (basis for e-bit cost)
- `solver/src/structures.rs` - Runtime types to extend
- `generator/build/parse.rs` - Built-in function table to extend
- `generator/build/emit.rs` - Code emitter to extend

---

## 10. Key References

- Generating Compilers for Qubit Mapping and Routing (Molavi et al., 2025): Section 4.2 (Amaro grammar), Section 5 (MaxState), Section 6 (case studies), Theorem 5.7
- Distributed Quantum Computing: a Survey
 (2022): Section 5.3 (TeleGate/TeleData), Section 6.1 (DQC compilation steps)
- A Multilevel Framework for Partitioning Quantum Circuits (Burt, Chen, Leung, 2026): Eq.(18)-(19) (e-bit cost), Algorithm 1 (gate grouping), Section 2.2 (remote gate taxonomy)

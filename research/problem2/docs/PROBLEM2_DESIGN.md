# Design: Problem 2 — Comm Qubit Budget-Aware Scheduling

This is a pointer file. The full design document lives at:

`~/Desktop/Amaro Documents/problem2_design.md`

It is kept there alongside the other research documents for this project.

## Quick Summary

**What:** Extend Amaro's ArchInfo with Bell pair generation hardware parameters
(`n_comm_qubits`, `bell_success_prob`, `bell_attempt_interval`, `max_bell_rate`, `code_distance`)
and enforce `max_bell_pairs_per_cycle` as a hard constraint in MaxState.

**Formula:** `R_Bell = P_aa × min(N_comm / τ_attempt, R_max)`

**Key insight:** Remote gate `realize_gate` returns `None` when budget exceeded → MaxState
naturally defers the gate to the next cycle. No MaxState rewrite needed.

**Single-QPU routing:** Completely unaffected (new fields default to no-constraint).

## Files in This Directory

- `../examples/dist_2qpu_budget.qmrl` — Example QMRL with comm qubit params
- `../examples/arch_2qpu_budget.json` — Example arch JSON (single cavity, L=20)
- `../benchmarks/` — Benchmark circuits for evaluation

## Related

- `research/distributed/docs/DESIGN.md` — Problem 1 design (must be complete first)
- `research/distributed/examples/simple_2qpu.qmrl` — Problem 1 QMRL (base for this file)

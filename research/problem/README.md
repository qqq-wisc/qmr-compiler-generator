# Problem : Communication Qubit Budget-Aware Scheduling

This directory contains research and implementation for adding Bell pair generation rate constraints to Amaro's distributed quantum routing — Problem Statement of the Amaro-DQC extension.

## The Problem

Current distributed quantum compilers (pytket-DQC, disqco) assume Bell pairs are available on-demand. In reality, Bell pair generation rate is bounded by hardware:

```
R_Bell(N_comm) = P_aa × min(N_comm / τ_attempt, R_max)
```

A schedule requiring 80 Bell pairs per cycle when hardware can only generate 40 is physically unimplementable — yet reported as valid by existing compilers.

## Directory Contents

- `docs/` — Design documents, notes, analysis
- `examples/` — QMRL files and architecture JSONs demonstrating comm qubit budgeting
- `benchmarks/` — Benchmark circuits for evaluation against pytket-DQC / disqco

## Status

- [ ] Phase 1: Language extension (symbols.rs, semantics.rs)
- [ ] Phase 2: Compiler parser + emitter (parse.rs, emit.rs)
- [ ] Phase 3: Runtime implementation (structures.rs + arch JSON loader)
- [ ] Phase 4: MaxState Bell pair budget enforcement
- [ ] Phase 5: Evaluation (5 benchmarks, comparison vs. baselines)

## Key Numbers (from Sinclair et al., arXiv:2408.08955)

| Hardware | P_aa | R_max | N_comm at saturation |
|----------|------|-------|----------------------|
| Free-space | 0.35% | N/A | N/A |
| Single optical cavity | 10% | 10 MHz | ~100–160 qubits |
| 30 micro-cavities | 24% | 286 MHz | ~90 qubits |

For L=20 surface code: baseline Bell pair demand = 2L = 40 pairs/cycle.

## Building on Problem 1

This branch (`feature/comm-qubit-budget-scheduling`) builds on the distributed routing infrastructure from `research/distributed-quantum-routing`:
- `qpu_id()`, `same_qpu()`: already designed
- `num_qpus`, `comm_qubits`, `alg_qubits` in ArchInfo: already designed
- `GateRealization.remote: Bool`: already designed

The Problem adds the Bell pair generation physics on top of this base.

## Important: Single-QPU Routing Unchanged

New fields are optional with backward-compatible defaults. All existing QMRL files (nisq, scmr, ilq, mqlss, etc.) and all extension tests pass unmodified.

## References

- Sinclair et al., "Fault-tolerant optical interconnects for neutral-atom arrays" Physical Review Research 7, 013313 (2025); arXiv:2408.08955
- Generating Compilers for Qubit Mapping and Routing: Molavi et al., arXiv:2508.10781v2 (2025)
- Design document: `docs/PROBLEM_DESIGN.md`

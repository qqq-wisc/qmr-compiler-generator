# Literature Review


## One-Line Summary Per Paper

| Paper | Models N_comm / R_Bell? | Gap |
|-------|-------------------------|-----|
| pytket-DQC | ❌ | No hardware params at all |
| DISQCO | ❌ | Total qubits only, no comm/data split |
| Sinclair et al. (arXiv:2408.08955) | ✅ | Hardware analysis, no compiler |
| Network-Aware Scheduling (arXiv:2504.20176) | ✅ (datacenter) | Not QEC-cycle aware |
| Lattice Surgery Resource Analysis (arXiv:2511.21885) | Partial | Tracks EPR count, not rate |
| Modular Arch FTQC (npj QI 2025) | ✅ (1 comm/module) | No optimization |
| UNIQ (arXiv:2512.00401) | ❌ | ILP, no hardware model |
| Lattice Surgery Bell Meas (arXiv:2510.13541) | Partial | Reduces count, not rate constraint |
| Hypergraph Partitioning (PRA 2019) | ❌ | Foundational, pre-hardware-aware |
| Hardware-SW Co-design (arXiv:2503.18329) | Partial | Wrong level of detail |

**Bottom line:** No existing compiler takes `N_comm`, `P_aa`, `τ_attempt`, `R_max` as inputs or enforces `R_Bell × T_cycle` as a hard scheduling constraint.

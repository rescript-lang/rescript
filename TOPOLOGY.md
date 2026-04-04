<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> -->

# TOPOLOGY.md — rescript

## Purpose

ReScript compiler fork/mirror providing fast, simple, fully-typed JavaScript compilation. Primary application language for hyperpolymath ecosystem, used in ReScript-TEA UIs (PanLL panels, Gossamer windows, Stapeln). This mirror enables ecosystem-specific enhancements and integration with formal verification tools.

## Module Map

```
rescript/
├── compiler/            # OCaml-based ReScript compiler
├── cli/                 # Command-line tools
├── analysis/            # Type analysis and optimization
├── vendor/              # Dependencies and third-party code
├── runtime/             # JavaScript runtime glue
├── docs/                # Language documentation
└── .github/workflows/   # CI/CD (build, test, release)
```

## Data Flow

```
[ReScript Source (.res)] ──► [Parser] ──► [Type Checker] ──► [Optimizer] ──► [JavaScript]
                                              ↓
                                        [Type Inference] ──► [Error Reporting]
```

## Ecosystem Integration

- **PanLL**: Panel UI framework built with ReScript-TEA
- **Gossamer**: Window management system
- **IDApTIK**: Game UI scripting (workarounds documented)
- **Developer tools**: Code intelligence, IDE integration

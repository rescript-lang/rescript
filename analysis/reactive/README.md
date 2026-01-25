# Reactive Collections Library

A library for incremental computation using reactive collections with delta-based updates.

## Overview

This library provides composable reactive collections that automatically propagate changes through a computation graph. When source data changes, only the affected parts of derived collections are recomputed.

### Key Features

- **Delta-based updates**: Changes propagate as `Set`, `Remove`, or `Batch` deltas
- **Glitch-free semantics**: Topological scheduling ensures consistent updates
- **Composable combinators**: `flatMap`, `join`, `union`, `fixpoint`
- **Incremental fixpoint**: Efficient transitive closure with support for additions and removals

## Usage

```ocaml
open Reactive

(* Create a source collection *)
let (files, emit) = source ~name:"files" ()

(* Derive collections with combinators *)
let decls = flatMap ~name:"decls" files
  ~f:(fun _path data -> data.declarations)
  ()

let refs = flatMap ~name:"refs" files
  ~f:(fun _path data -> data.references)
  ~merge:PosSet.union
  ()

(* Join collections *)
let resolved = join ~name:"resolved" refs decls
  ~key_of:(fun pos _ref -> pos)
  ~f:(fun pos ref decl_opt -> ...)
  ()

(* Compute transitive closure *)
let reachable = fixpoint ~name:"reachable"
  ~init:roots
  ~edges:graph
  ()

(* Emit changes *)
emit (Set ("file.res", file_data))
emit (Batch [set "a.res" data_a; set "b.res" data_b])
```

## Combinators

| Combinator | Description |
|------------|-------------|
| `source` | Create a mutable source collection |
| `flatMap` | Transform and flatten entries, with optional merge |
| `join` | Look up keys from left collection in right collection |
| `union` | Combine two collections, with optional merge for conflicts |
| `fixpoint` | Compute transitive closure incrementally |

## Building & Testing

```bash
# Build the library
make build

# Run all tests
make test

# Clean build artifacts
make clean
```

## Test Structure

Tests are organized by theme:

| File | Description |
|------|-------------|
| `FlatMapTest.ml` | FlatMap combinator tests |
| `JoinTest.ml` | Join combinator tests |
| `UnionTest.ml` | Union combinator tests |
| `FixpointBasicTest.ml` | Basic fixpoint graph traversal |
| `FixpointIncrementalTest.ml` | Incremental fixpoint updates |
| `BatchTest.ml` | Batch processing tests |
| `IntegrationTest.ml` | End-to-end file processing |
| `GlitchFreeTest.ml` | Glitch-free scheduler tests |

## Glitch-Free Semantics

The scheduler ensures that derived collections never see inconsistent intermediate states:

1. **Topological levels**: Each node has a level based on its dependencies
2. **Accumulate phase**: All deltas at a level are collected before processing
3. **Propagate phase**: Nodes process accumulated deltas in level order

This prevents issues like:
- Anti-joins seeing partial data (e.g., refs without matching decls)
- Multi-level unions causing spurious additions/removals

## Usage in Reanalyze

This library powers the reactive dead code analysis in reanalyze:

- `ReactiveFileCollection`: Manages CMT file processing
- `ReactiveMerge`: Merges per-file data into global collections
- `ReactiveLiveness`: Computes live declarations via fixpoint
- `ReactiveSolver`: Generates dead code issues reactively


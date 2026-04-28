# Incremental Fixpoint for Reactive Analysis

## Introduction

When a source file changes in a codebase, analysis results that depend on it may become stale. Recomputing the entire analysis from scratch is correct but wasteful: typically only a small region of the dependency graph is affected. The incremental fixpoint algorithm described here maintains the set of reachable nodes in a dynamic directed graph, updating it efficiently as roots and edges change. It is the core propagation engine behind the reactive analysis server.

This report covers the problem definition, the algorithm and its key design ideas, correctness instrumentation, and empirical results from a replay-based evaluation on a real codebase.

## Problem Definition

The algorithm maintains transitive reachability over a directed graph that evolves over time.

**State.** The graph is defined by:

- A set of **roots** R, the starting points of reachability.
- An **edge relation** E mapping each node to its list of successors.
- A **reachable set** C, the nodes reachable from R through E.

The fundamental invariant is:

> **C = Reach(R, E)** — the least fixed point of forward reachability from R through E.

**Updates.** The graph changes in discrete *waves*. Each wave supplies:

- **Root updates**: a list of `(k, unit option)` entries. `Some ()` adds a root; `None` removes one.
- **Edge updates**: a list of `(k, k list option)` entries. `Some succs` sets the successors of k; `None` removes all edges from k.

**Output.** Each wave returns a list of *delta entries* describing how C changed:

- `(k, Some ())` — node k became reachable (was not in C before, is now).
- `(k, None)` — node k became unreachable (was in C before, is no longer).

The output represents the *net effect* of the wave: if a node is tentatively removed and then recovered within the same wave, no output is emitted for it.

## Data Structures

The algorithm maintains four mutable structures:

| Structure | Purpose |
|-----------|---------|
| `current` | The reachable set C (hash set of nodes) |
| `roots` | The root set R (hash set) |
| `edge_map` | Forward edges: node -> successor list |
| `pred_map` | Reverse edges: node -> set of predecessors |

The predecessor map (`pred_map`) is the key auxiliary structure. It enables efficient *support checking*: given a tentatively deleted node, we can quickly determine whether any live predecessor still points to it, without scanning the entire graph. The cost of maintaining `pred_map` is proportional to edge updates: each edge addition or removal requires a constant-time insert or delete in the predecessor set of the target node. This is amortized across normal edge-update processing and adds no asymptotic overhead.

## Algorithm

### Design Intuition

When edges or roots are removed, some previously reachable nodes may lose all paths from the roots. The challenge is determining *which* nodes are still reachable without recomputing from scratch.

The algorithm uses a **delete-then-rederive** strategy:

1. **Delete pessimistically** — starting from invalidation points (removed roots, targets of removed edges), propagate tentative deletions forward through the *old* edges. This marks every node that *might* have lost reachability.

2. **Rederive optimistically** — scan the tentatively deleted nodes and recover those that still have *support* in the *new* state: either they are roots, or at least one live predecessor still points to them. Recovered nodes propagate support to their successors.

3. **Expand forward** — from newly added roots and nodes with new outgoing edges, discover newly reachable nodes via forward BFS.

This two-pass approach (pessimistic deletion followed by optimistic recovery) is both simple and correct: it avoids the complexity of trying to determine "true" deletions upfront, while ensuring the final state is exactly the least fixed point.

### Phases in Detail

The `apply` function executes one wave in the following phases:

**Phase 1 — Analyze changes.** Before modifying any state, examine each edge update against the current edge map. For each updated source node, compute which successor edges were removed (`removed_targets`) and whether any new edges were added (`has_new_edge`). This pre-analysis drives the deletion and expansion phases.

**Phase 2 — Tentative deletion.** Seed a deletion queue with:
- Roots being removed (if they were in C).
- Targets of removed edges (if they were in C).

Propagate forward through the *old* edges: for each node popped from the queue, enqueue its old successors that are still in C. This produces `deleted_nodes`, a set of nodes that may have lost reachability.

The deletion uses old edges deliberately: we need to follow the paths that *used to* provide reachability, since those are the paths that may have broken.

Next, apply root and edge updates to the state (updating `roots`, `edge_map`, and `pred_map`), then remove all deleted nodes from `current`.

**Phase 3 — Re-derivation.** Scan the deleted nodes. A deleted node is *supported* if:
- It is a root (k in R), OR
- It has at least one predecessor p such that p is in `current` and k is in E(p).

Supported nodes are added back to `current` and their successors are checked recursively. This phase only adds nodes back; it never removes them. When it converges, every deleted node that had an alternative reachability path has been recovered.

**Phase 4 — Expansion.** Starting from newly added roots and nodes whose edge updates introduced new successors, perform forward BFS to discover all newly reachable nodes. Each newly reached node is added to `current` and an add-delta is emitted — unless the node was tentatively deleted earlier in this same wave (in which case it is a no-op recovery, not a net change).

**Phase 5 — Emit removals.** For each node in `deleted_nodes` that is *not* in the final `current`, emit a remove-delta. These are the nodes that were genuinely lost.

### Worked Example

Consider a graph with root A and edges A->B, A->X, B->D, X->D:

```
    A (root)
   / \
  B   X
   \ /
    D
```

The reachable set is C = {A, B, X, D}.

**Wave: remove the edge A->B.**

1. *Analyze*: Edge A->B is removed. `removed_targets = [B]`, the target of the dropped edge.

2. *Tentative deletion*: Seed the delete queue with B (target of removed edge, currently in C). Propagate forward through old edges: B->D, so D is also tentatively deleted. `deleted_nodes = {B, D}`. Remove B and D from `current`. Now `current = {A, X}`.

3. *Re-derivation*: Check each deleted node for support.
   - B: not a root. Predecessors of B = {A}. Is A in current? Yes. Is B in A's *new* successors? A's new edges are [X] (we removed A->B). So A does not point to B anymore. B is **not supported**. B stays deleted.
   - D: not a root. Predecessors of D = {B, X}. B is not in current. X is in current. Is D in X's successors? X->D exists and was not modified. D **is supported**. Add D back to `current`.

   After re-derivation: `current = {A, X, D}`.

4. *Expansion*: No new roots or new edges added. Nothing to expand.

5. *Emit removals*: `deleted_nodes = {B, D}`. D is back in `current`, so no removal for D. B is not in `current`. Emit `(B, None)`.

**Output: `[(B, None)]`** — only B was lost. D survived because it had an alternative path through X.

### Complexity

- **Best case** (additions only, no deletions): work is proportional to the number of newly reachable nodes.
- **Typical case** (localized changes): work is proportional to the affected region of the graph — the tentatively deleted subgraph plus newly reachable nodes.
- **Worst case** (removing a root that reaches everything): degrades to a full BFS over the entire graph.

Space is O(|C| + |E|) for the reachable set, edge map, and predecessor map.

## Correctness Instrumentation

Debug invariants are implemented in the `Invariants` submodule and enabled by setting `RESCRIPT_REACTIVE_FIXPOINT_ASSERT=1`. They validate the algorithm's internal consistency after each phase:

1. **Edge change consistency** — `removed_targets` and `has_new_edge` match the actual diff between old and new successor lists.
2. **Deletion closure** — deleted nodes form a forward-closed set under the old edges (no reachable successor of a deleted node is left undeleted).
3. **Post-deletion state** — `current` equals the pre-wave reachable set minus `deleted_nodes`.
4. **Re-derivation convergence** — no supported node remains outside `current` after Phase 3.
5. **Removal output correctness** — emitted removal deltas match `deleted_nodes \ current` (nodes deleted but not recovered).
6. **Final closure and delta correctness** — `current` equals `Reach(R, E)` computed by a fresh BFS, and the full output (both adds and removes) matches the set difference between pre-wave and post-wave `current`.

These checks add significant overhead and are intended for testing and validation, not production use.

## Empirical Evaluation

### Setup

The algorithm was evaluated using a replay-based benchmark. The workload replays 56 sequential commits from a real project (Hyperindex), with invariant assertions and metrics collection enabled throughout. For each commit, the script runs the incremental (server-backed) analysis and a cold baseline that recomputes the full analysis from scratch (with `RESCRIPT_REANALYZE_NO_SERVER=1`), comparing both for timing and failure behavior.

Average change size per commit: 1.3 files, 5.9 insertions, 47.2 deletions (range: 1–5 changed files per commit).

### Results

**Fixpoint internal metrics** (aggregated over 56 waves):

| Category | Metric | Value |
|----------|--------|-------|
| **Throughput** | Root entries processed | 9,926 |
| | Edge entries processed | 46,836 |
| | Output deltas emitted | 5,241 |
| **Deletion/recovery** | Nodes tentatively deleted | 3,465 |
| | Nodes re-derived (recovered) | 874 |
| | Waves involving re-derivation | 34 / 56 (61%) |
| **Incremental efficiency** | Edge work vs full recompute | 16.7% |
| | Node work vs full recompute | 95.5% |
| **Per-wave maxima** | Max root entries | 330 |
| | Max edge entries | 1,670 |
| | Max deleted nodes | 574 |
| | Max re-derived nodes | 133 |

### Interpretation

**Re-derivation is central, not exceptional.** 61% of waves trigger the delete-then-rederive path. The worked example above (where D survives deletion because of an alternative path through X) is the common case in real dependency graphs, not a corner case.

**Edge-work savings are substantial.** The incremental algorithm traverses only 16.7% of the edges that a full recomputation would visit — an 83% reduction. This confirms that localized source changes produce localized graph updates.

**Node-work savings are modest.** Node-side bookkeeping (queue operations, hash table lookups) runs at 95.5% of the full baseline. This is likely because the node-work counter counts queue pops and hash table membership checks, which are O(1) operations that dominate BFS cost regardless of how many edges are skipped. In other words, the algorithm successfully avoids traversing most edges, but still touches most nodes during the deletion and re-derivation sweeps. Reducing this overhead would require more targeted invalidation — for example, bounding the deletion frontier using dominator information or topological depth — rather than optimizing individual operations.

**Output is much smaller than input.** 56,762 input entries produce only 5,241 output deltas — a 10.8x compression ratio. Most of the internal work cancels out, consistent with the high re-derivation rate.

## Comparison to Alternatives

Several strategies exist for maintaining transitive reachability under updates:

- **Full recomputation**: Rerun BFS/DFS from all roots after every change. Correct and simple, but wasteful when changes are localized. The empirical results show the incremental algorithm traverses only 16.7% of the edges that full recomputation would visit.

- **Reference counting**: Track the number of paths reaching each node and remove it when the count drops to zero. This is efficient for pure deletions but fails in the presence of cycles (counts never reach zero) and does not naturally handle the case where a node loses one path but retains another through a different predecessor — precisely the re-derivation scenario that occurs in 61% of waves here.

- **DFS-based marking**: Mark nodes as "dirty" and re-verify reachability via DFS from roots. This avoids false deletions but may revisit large portions of the graph when many nodes are marked dirty, offering less control over the re-derivation scope than the delete-then-rederive approach.

The delete-then-rederive strategy used here combines the simplicity of pessimistic deletion with efficient recovery via the predecessor map, avoiding the cycle problems of reference counting and the unbounded re-traversal of DFS marking.

## Limitations

1. Results are from a single replay corpus and commit ordering. Different projects or change patterns may yield different efficiency ratios.
2. Work counters (node pops, edge scans) are algorithmic proxies, not hardware-level measurements.

## Conclusion

The incremental fixpoint algorithm maintains exact transitive closure through a delete-then-rederive strategy that is both simple to reason about and effective in practice. Assertion-enabled replay over 56 real commits confirms correctness, while metrics show an 83% reduction in edge traversals compared to full recomputation. The data also reveals that re-derivation is a routine operation (occurring in 61% of waves) rather than a rare edge case, validating the algorithm's two-pass design.

The primary optimization opportunity lies in reducing node-side bookkeeping overhead, which currently runs near the full-recomputation baseline despite the large edge-work savings.

# Reactive Reanalyze: Incremental Analysis Design Notes

## Executive Summary

This document is an early design exploration of making `reanalyze` incremental: keeping state between runs and reacting to file changes.

**Note**: The current implementation lives in `analysis/reactive/` and `analysis/reanalyze/src/Reactive*` and differs from some details below (e.g. no `-parallel` flag; caching is implemented in OCaml, not via a C++ `Marshal_cache`).

## Current Architecture

### Reanalyze Processing Flow

```
                        ┌─────────────────┐
                        │ Collect CMT     │
                        │ File Paths      │
                        └────────┬────────┘
                                 │
                        ┌────────▼────────┐
                        │ Load CMT Files  │ ← 77% of time (~780ms)
                        │ (Cmt_format.    │   
                        │  read_cmt)      │
                        └────────┬────────┘
                                 │
                        ┌────────▼────────┐
                        │ Process Each    │
                        │ File → file_data│
                        └────────┬────────┘
                                 │
              ┌─────────────────┴─────────────────┐
              │                                    │
     ┌────────▼────────┐                 ┌────────▼────────┐
     │ Merge Builders  │                 │ Exception       │
     │ (annotations,   │                 │ Results         │
     │  decls, refs,   │                 └─────────────────┘
     │  cross_file,    │
     │  file_deps)     │ ← 8% of time (~80ms)
     └────────┬────────┘
              │
     ┌────────▼────────┐
     │ Solve (DCE,     │ ← 15% of time (~150ms)
     │ optional args)  │
     └────────┬────────┘
              │
     ┌────────▼────────┐
     │ Report Issues   │ ← <1% of time
     └─────────────────┘
```

### Current Bottleneck

From the benchmark (50 copies, ~4900 files):

| Phase | Time | % of Total |
|-------|------|------------|
| File loading | ~779ms | ~77% |
| Merging | ~81ms | ~8% |
| Solving | ~146ms | ~15% |
| Total | ~1007ms | 100% |

**CMT file loading is the dominant cost** because each file requires:
1. System call to open file
2. Reading marshalled data from disk
3. Unmarshalling into OCaml heap
4. AST traversal to extract analysis data

## Proposed Architecture: Reactive Analysis Service

### Design Goals

1. **Persistent service** - Stay running and maintain state between analysis runs
2. **File watching** - React to file changes (create/modify/delete)
3. **Incremental updates** - Only process changed files
4. **Cached results** - Keep processed `file_data` in memory
5. **Fast iteration** - Sub-10ms response for typical edits

### Integration via reactive collections

The implementation uses reactive collections and file-backed collections to cache processed per-file results and propagate changes incrementally.

#### `ReactiveFileCollection` - Delta-Based Processing

```ocaml
(* Create collection that maps CMT paths to processed file_data *)
let cmt_collection = ReactiveFileCollection.create
  ~process:(fun (cmt_infos : Cmt_format.cmt_infos) ->
    (* This is called only when file changes *)
    process_cmt_for_dce ~config cmt_infos
  )

(* Initial load - process all files once *)
List.iter (ReactiveFileCollection.process_file cmt_collection) all_cmt_paths

(* On file watcher event - only process changed files *)
(* In practice, reanalyze uses batch processing for bulk load and explicit remove/add
   operations for churn testing. *)

(* Get all processed data for analysis *)
let file_data_list = ReactiveFileCollection.values cmt_collection
```

### Service Architecture

```
┌────────────────────────────────────────────────────────────────┐
│                    Reanalyze Service                            │
├────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────┐      ┌─────────────────────────────────┐     │
│  │ File Watcher │─────▶│ Reactive_file_collection        │     │
│  │ (fswatch/    │      │ ┌───────────────────────────┐   │     │
│  │  inotify)    │      │ │ path → file_data cache    │   │     │
│  └──────────────┘      │ │ (backed by Marshal_cache) │   │     │
│                        │ └───────────────────────────┘   │     │
│                        └──────────┬──────────────────────┘     │
│                                   │                             │
│                                   │ file_data_list              │
│                                   ▼                             │
│                        ┌─────────────────────────────────┐     │
│                        │ Incremental Merge & Solve       │     │
│                        │ (may be reactive in future)     │     │
│                        └──────────┬──────────────────────┘     │
│                                   │                             │
│                                   ▼                             │
│                        ┌─────────────────────────────────┐     │
│                        │ Issues / Reports                │     │
│                        └─────────────────────────────────┘     │
│                                                                  │
└────────────────────────────────────────────────────────────────┘
```

### API Design

```ocaml
module ReactiveReanalyze : sig
  type t
  (** A reactive analysis service *)

  val create : config:DceConfig.t -> project_root:string -> t
  (** Create a new reactive analysis service *)

  val start : t -> unit
  (** Start file watching and initial analysis *)

  val stop : t -> unit
  (** Stop file watching *)

  val analyze : t -> AnalysisResult.t
  (** Run analysis on current state. Fast if no files changed. *)

  val on_file_change : t -> string -> unit
  (** Notify of a file change (for external file watchers) *)

  val apply_events : t -> Reactive_file_collection.event list -> unit
  (** Apply batch of file events *)
end
```

## Performance Analysis

### Expected Speedup

| Scenario | Current | With skip-lite | Speedup |
|----------|---------|----------------|---------|
| Cold start (all files) | 780ms | 780ms | 1x |
| Warm cache, no changes | 780ms | ~20ms | **39x** |
| Single file changed | 780ms | ~2ms | **390x** |
| 10 files changed | 780ms | ~15ms | **52x** |

### How skip-lite Achieves This

1. **Marshal_cache.with_unmarshalled_if_changed**:
   - Stats all files to check modification time (~20ms for 5000 files)
   - Only unmarshals files that changed
   - Returns `None` for unchanged files, `Some result` for changed

2. **Reactive_file_collection**:
   - Maintains hash table of processed values
   - On `apply`, only processes files in the event list
   - Iteration is O(n) but values are already computed

### Memory Considerations

| Data | Storage | GC Impact |
|------|---------|-----------|
| CMT file bytes | mmap (off-heap) | None |
| Unmarshalled cmt_infos | OCaml heap (temporary) | During callback only |
| Processed file_data | OCaml heap (cached) | Scanned by GC |

For 5000 files with average 20KB each:
- mmap cache: ~100MB (off-heap, OS-managed)
- file_data cache: ~50MB (on-heap, estimate)

## Implementation Plan

### Phase 1: Integration Setup

1. **Add skip-lite dependency** to dune/opam
2. **Create wrapper module** `CmtCache` that provides:
   ```ocaml
   val read_cmt : string -> Cmt_format.cmt_infos
   (** Drop-in replacement for Cmt_format.read_cmt using Marshal_cache *)
   ```

### Phase 2: Reactive Collection

1. **Define file_data type** as the cached result type
2. **Create reactive collection** for CMT → file_data mapping
3. **Implement delta processing** that only reprocesses changed files

### Phase 3: Analysis Service

1. **File watching integration** (can use fswatch, inotify, or external watcher)
2. **Service loop** that waits for events and re-runs analysis
3. **LSP integration** (optional) for editor support

### Phase 4: Incremental Merge & Solve (Future)

The current merge and solve phases are relatively fast (22% of time), but could be made incremental in the future:

- Track which declarations changed
- Incrementally update reference graph
- Re-solve only affected transitive closure

## Prototype Implementation

Here's a minimal prototype showing how to integrate `Reactive_file_collection`:

```ocaml
(* reactive_analysis.ml *)

module CmtCollection = struct
  type file_data = DceFileProcessing.file_data

  let collection : file_data Reactive_file_collection.t option ref = ref None

  let init ~config ~cmt_paths =
    let coll = Reactive_file_collection.create
      ~process:(fun (cmt_infos : Cmt_format.cmt_infos) ->
        (* Extract file context from cmt_infos *)
        let source_path = 
          match cmt_infos.cmt_annots |> FindSourceFile.cmt with
          | Some path -> path
          | None -> failwith "No source file"
        in
        let module_name = Paths.getModuleName source_path in
        let is_interface = match cmt_infos.cmt_annots with
          | Cmt_format.Interface _ -> true
          | _ -> false
        in
        let file : DceFileProcessing.file_context = {
          source_path; module_name; is_interface
        } in
        let cmtFilePath = "" (* not used in process_cmt_file body *) in
        DceFileProcessing.process_cmt_file ~config ~file ~cmtFilePath cmt_infos
      )
    in
    (* Initial load *)
    List.iter (Reactive_file_collection.add coll) cmt_paths;
    collection := Some coll;
    coll

  let apply_events events =
    match !collection with
    | Some coll -> Reactive_file_collection.apply coll events
    | None -> failwith "Collection not initialized"

  let get_all_file_data () =
    match !collection with
    | Some coll -> Reactive_file_collection.values coll
    | None -> []
end

(* Modified Reanalyze.runAnalysis *)
let runAnalysisIncremental ~config ~events =
  (* Apply only the changed files *)
  CmtCollection.apply_events events;
  
  (* Get all file_data (instant - values already computed) *)
  let file_data_list = CmtCollection.get_all_file_data () in
  
  (* Rest of analysis is same as before *)
  let annotations, decls, cross_file, refs, file_deps =
    merge_all_builders file_data_list
  in
  solve ~annotations ~decls ~refs ~file_deps ~config
```

## Testing Strategy

1. **Correctness**: Verify reactive analysis produces same results as batch
2. **Performance**: Benchmark incremental updates vs full analysis
3. **Edge cases**: 
   - File deletion during analysis
   - Rapid successive changes
   - Build errors (incomplete CMT files)

## Open Questions

1. **Build system integration**: How to get file events from rewatch?
2. **CMT staleness**: What if build system is still writing CMT files?
3. **Multi-project**: How to handle monorepos with multiple rescript.json?
4. **Memory limits**: When to evict file_data from cache?

## Integration Points

### 1. Shared.tryReadCmt → Marshal_cache

Current code in `analysis/src/Shared.ml`:
```ocaml
let tryReadCmt cmt =
  if not (Files.exists cmt) then (
    Log.log ("Cmt file does not exist " ^ cmt);
    None)
  else
    match Cmt_format.read_cmt cmt with
    | exception ... -> None
    | x -> Some x
```

With Marshal_cache:
```ocaml
let tryReadCmt cmt =
  if not (Files.exists cmt) then (
    Log.log ("Cmt file does not exist " ^ cmt);
    None)
  else
    try 
      Some (Marshal_cache.with_unmarshalled_file cmt Fun.id)
    with Marshal_cache.Cache_error (_, msg) ->
      Log.log ("Invalid cmt format " ^ cmt ^ ": " ^ msg);
      None
```

### 2. Reanalyze.loadCmtFile → Reactive_file_collection

Current code in `analysis/reanalyze/src/Reanalyze.ml`:
```ocaml
let loadCmtFile ~config cmtFilePath : cmt_file_result option =
  let cmt_infos = Cmt_format.read_cmt cmtFilePath in
  ...
```

With reactive collection:
```ocaml
(* Global reactive collection *)
let cmt_collection : cmt_file_result Reactive_file_collection.t option ref = ref None

let init_collection ~config =
  cmt_collection := Some (Reactive_file_collection.create
    ~process:(fun (cmt_infos : Cmt_format.cmt_infos) ->
      process_cmt_infos ~config cmt_infos
    ))

let loadCmtFile_reactive ~config cmtFilePath =
  match !cmt_collection with
  | Some coll -> Reactive_file_collection.get coll cmtFilePath
  | None -> loadCmtFile ~config cmtFilePath  (* fallback *)
```

### 3. File Watcher Integration

The analysis server already has `DceCommand.ml`. We can extend it to a service:

```ocaml
(* DceService.ml *)

type t = {
  config: Reanalyze.DceConfig.t;
  collection: cmt_file_result Reactive_file_collection.t;
  mutable last_result: Reanalyze.AnalysisResult.t option;
}

let create ~project_root =
  let config = Reanalyze.DceConfig.current () in
  let cmt_paths = Reanalyze.collectCmtFilePaths ~cmtRoot:None in
  let collection = Reactive_file_collection.create
    ~process:(process_cmt_for_config ~config)
  in
  List.iter (Reactive_file_collection.add collection) cmt_paths;
  { config; collection; last_result = None }

let on_file_change t events =
  Reactive_file_collection.apply t.collection events;
  (* Invalidate cached result *)
  t.last_result <- None

let analyze t =
  match t.last_result with
  | Some result -> result  (* Cached, no files changed *)
  | None ->
    let file_data_list = Reactive_file_collection.values t.collection in
    let result = run_analysis_on_file_data ~config:t.config file_data_list in
    t.last_result <- Some result;
    result
```

### 4. Build System Integration (rewatch)

Rewatch already watches for file changes. We can extend it to notify the analysis service:

In `rewatch/src/watcher.rs`:
```rust
// After successful compilation of a module
if let Some(analysis_socket) = &state.analysis_socket {
    analysis_socket.send(AnalysisEvent::Modified(cmt_path));
}
```

Or via a Unix domain socket/named pipe that the analysis service listens on.

## Dependency Setup

Add to `analysis/dune`:
```dune
(library
 (name analysis)
 (libraries
  ...
  skip-lite.marshal_cache
  skip-lite.reactive_file_collection))
```

Add to `analysis.opam`:
```opam
depends: [
  ...
  "skip-lite" {>= "0.1"}
]
```

## Conclusion

Integrating skip-lite's reactive collections with reanalyze offers a path to **39-390x speedup** for incremental analysis. The key insight is that CMT file loading (77% of current time) can be eliminated for unchanged files, and the processed file_data can be cached.

The implementation requires:
1. Adding skip-lite as a dependency
2. Wrapping CMT loading with Marshal_cache (immediate benefit: mmap caching)
3. Creating reactive collection for file_data (benefit: only process changed files)
4. Creating a service mode that watches for file changes (benefit: persistent state)

The merge and solve phases (23% of time) remain unchanged initially, but could be made incremental in the future for even greater speedups.

## Next Steps

1. **Phase 0**: Add skip-lite as optional dependency (behind a feature flag)
2. **Phase 1**: Replace `Cmt_format.read_cmt` with `Marshal_cache` wrapper
3. **Phase 2**: Benchmark improvement from mmap caching alone
4. **Phase 3**: Implement `Reactive_file_collection` for file_data
5. **Phase 4**: Create analysis service with file watching
6. **Phase 5**: Integrate with rewatch for automatic updates


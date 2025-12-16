(** Abstraction over file dependency storage.

    Allows the solver to work with either:
    - [Frozen]: Traditional [FileDeps.t] (copied from reactive)
    - [Reactive]: Direct reactive collections (no copy, zero-cost on warm runs) *)

type t =
  | Frozen of FileDeps.t
  | Reactive of {
      files: (string, unit) Reactive.t;
      deps: (string, FileSet.t) Reactive.t;
    }

let of_frozen fd = Frozen fd

let of_reactive ~files ~deps = Reactive {files; deps}

let get_deps t file =
  match t with
  | Frozen fd -> FileDeps.get_deps fd file
  | Reactive r -> (
    match Reactive.get r.deps file with
    | Some s -> s
    | None -> FileSet.empty)

let iter_deps t f =
  match t with
  | Frozen fd -> FileDeps.iter_deps fd f
  | Reactive r -> Reactive.iter f r.deps

(** Topological iteration from roots to leaves.
    Works for both frozen and reactive - builds temporary structures as needed. *)
let iter_files_from_roots_to_leaves t iterFun =
  match t with
  | Frozen fd -> FileDeps.iter_files_from_roots_to_leaves fd iterFun
  | Reactive r ->
    (* Build temporary FileDeps.t from reactive collections for topo sort *)
    let files = ref FileSet.empty in
    Reactive.iter (fun f () -> files := FileSet.add f !files) r.files;
    let deps = FileDeps.FileHash.create 256 in
    Reactive.iter
      (fun from_file to_files ->
        FileDeps.FileHash.replace deps from_file to_files)
      r.deps;
    let fd = FileDeps.create ~files:!files ~deps in
    FileDeps.iter_files_from_roots_to_leaves fd iterFun

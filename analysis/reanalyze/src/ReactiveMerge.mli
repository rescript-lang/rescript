(** Reactive merge of per-file DCE data into global collections.

    Given a reactive collection of (path, file_data), this creates derived
    reactive collections that automatically update when source files change.

    {2 Example}

    {[
      (* Create reactive file collection *)
      let files = ReactiveAnalysis.create ~config in

      (* Process files *)
      ReactiveAnalysis.process_files ~collection:files ~config paths;

      (* Create reactive merge from processed file data *)
      let merged = ReactiveMerge.create (ReactiveAnalysis.to_collection files) in

      (* Access derived collections *)
      Reactive.iter (fun pos decl -> ...) merged.decls;

      (* Or freeze for solver *)
      let decls = ReactiveMerge.freeze_decls merged in
    ]} *)

(** {1 Types} *)

type t = {
  decls: (Lexing.position, Decl.t) Reactive.t;
  annotations: (Lexing.position, FileAnnotations.annotated_as) Reactive.t;
  value_refs_from: (Lexing.position, PosSet.t) Reactive.t;
      (** Value refs: source -> targets *)
  type_refs_from: (Lexing.position, PosSet.t) Reactive.t;
      (** Type refs: source -> targets *)
  cross_file_items: (string, CrossFileItems.t) Reactive.t;
  file_deps_map: (string, FileSet.t) Reactive.t;
  files: (string, unit) Reactive.t;
  (* Reactive type/exception dependencies *)
  type_deps: ReactiveTypeDeps.t;
  exception_refs: ReactiveExceptionRefs.t;
}
(** All derived reactive collections from per-file data *)

(** {1 Creation} *)

val create : (string, DceFileProcessing.file_data option) Reactive.t -> t
(** Create reactive merge from a file data collection.
    All derived collections update automatically when source changes. *)

(** {1 Conversion to solver-ready format} *)

val freeze_decls : t -> Declarations.t
(** Convert reactive decls to Declarations.t for solver *)

val freeze_annotations : t -> FileAnnotations.t
(** Convert reactive annotations to FileAnnotations.t for solver *)

val freeze_refs : t -> References.t
(** Convert reactive refs to References.t for solver *)

val collect_cross_file_items : t -> CrossFileItems.t
(** Collect all cross-file items *)

val freeze_file_deps : t -> FileDeps.t
(** Convert reactive file deps to FileDeps.t for solver *)

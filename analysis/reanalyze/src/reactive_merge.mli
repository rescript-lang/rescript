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
  annotations: (Lexing.position, File_annotations.annotated_as) Reactive.t;
  value_refs_from: (Lexing.position, Pos_set.t) Reactive.t;
      (** Value refs: source -> targets *)
  type_refs_from: (Lexing.position, Pos_set.t) Reactive.t;
      (** Type refs: source -> targets *)
  cross_file_items: (string, Cross_file_items.t) Reactive.t;
  file_deps_map: (string, File_set.t) Reactive.t;
  files: (string, unit) Reactive.t;
  (* Reactive type/exception dependencies *)
  type_deps: Reactive_type_deps.t;
  exception_refs: Reactive_exception_refs.t;
}
(** All derived reactive collections from per-file data *)

(** {1 Creation} *)

val create : (string, Dce_file_processing.file_data option) Reactive.t -> t
(** Create reactive merge from a file data collection.
    All derived collections update automatically when source changes. *)

(** {1 Conversion to solver-ready format} *)

val freeze_decls : t -> Declarations.t
(** Convert reactive decls to Declarations.t for solver *)

val freeze_annotations : t -> File_annotations.t
(** Convert reactive annotations to FileAnnotations.t for solver *)

val freeze_refs : t -> References.t
(** Convert reactive refs to References.t for solver *)

val collect_cross_file_items : t -> Cross_file_items.t
(** Collect all cross-file items *)

val freeze_file_deps : t -> File_deps.t
(** Convert reactive file deps to FileDeps.t for solver *)

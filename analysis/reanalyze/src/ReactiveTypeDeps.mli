(** Reactive type-label dependencies.

    Expresses the type-label dependency computation as a reactive pipeline.
    When declarations change, only affected refs are recomputed.

    {2 Pipeline}

    {[
      decls
        |> (flatMap) decl_by_path     (* index by path *)
        |> (flatMap) same_path_refs   (* connect same-path duplicates *)
        |
        +-> (join) cross_file_refs    (* connect impl <-> intf *)
        |
        +-> all_type_refs             (* combined refs *)
    ]}

    {2 Example}

    {[
      let reactive_decls = ReactiveMerge.create ... in
      let type_deps = ReactiveTypeDeps.create
        ~decls:reactive_decls.decls
        ~report_types_dead_only_in_interface:true
      in
      (* Type refs update automatically when decls change *)
      ReactiveTypeDeps.add_to_refs_builder type_deps ~refs:my_refs_builder
    ]} *)

(** {1 Types} *)

type t
(** Reactive type-label dependency collections *)

(** {1 Creation} *)

val create :
  decls:(Lexing.position, Decl.t) Reactive.t ->
  report_types_dead_only_in_interface:bool ->
  t
(** Create reactive type-label dependencies from a decls collection.
    
    When the [decls] collection changes, type refs automatically update.
    
    [report_types_dead_only_in_interface] controls whether refs are bidirectional
    (false) or only intf->impl (true). *)

(** {1 Freezing} *)

val add_to_refs_builder : t -> refs:References.builder -> unit
(** Add all computed type refs to a References.builder.
    
    Call this after processing files to get the current type refs.
    The builder will contain all type-label dependency refs. *)


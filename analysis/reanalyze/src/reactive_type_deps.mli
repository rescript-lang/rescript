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
        +-> (join) manifest_refs      (* connect re-export equations *)
        |
        +-> all_type_refs_from        (* combined refs, refs_from direction *)
    ]}

    {2 Example}

    {[
      let reactive_decls = ReactiveMerge.create ... in
      let type_deps = ReactiveTypeDeps.create
        ~decls:reactive_decls.decls
        ~report_types_dead_only_in_interface:true
      in
      (* Type refs update automatically when decls change *)
      type_deps.all_type_refs_from
    ]} *)

(** {1 Types} *)

type t = {
  decl_by_path: (Dce_path.t, decl_info list) Reactive.t;
  (* refs_to direction: target -> sources *)
  same_path_refs: (Lexing.position, Pos_set.t) Reactive.t;
  cross_file_refs: (Lexing.position, Pos_set.t) Reactive.t;
  manifest_refs: (Lexing.position, Pos_set.t) Reactive.t;
  all_type_refs: (Lexing.position, Pos_set.t) Reactive.t;
  impl_to_intf_refs_path2: (Lexing.position, Pos_set.t) Reactive.t;
  intf_to_impl_refs: (Lexing.position, Pos_set.t) Reactive.t;
  (* refs_from direction: source -> targets (for forward solver) *)
  all_type_refs_from: (Lexing.position, Pos_set.t) Reactive.t;
}
(** Reactive type-label dependency collections *)

and decl_info = {
  pos: Lexing.position;
  pos_end: Lexing.position;
  path: Dce_path.t;
  is_interface: bool;
}
(** Simplified decl info for type-label processing *)

(** {1 Creation} *)

val create :
  decls:(Lexing.position, Decl.t) Reactive.t ->
  report_types_dead_only_in_interface:bool ->
  t
(** Create reactive type-label dependencies from a decls collection.
    
    When the [decls] collection changes, type refs automatically update.
    
    [report_types_dead_only_in_interface] controls whether refs are bidirectional
    (false) or only intf->impl (true). *)

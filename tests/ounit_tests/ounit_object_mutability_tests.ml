(* Representation-level tests for object-field mutability state: the
   linkable [field_mutability] cells in [Tfield] (doc §6.7). These properties
   are not observable from generated JavaScript, so they are tested here
   directly against [Ctype]/[Btype].

   The invariants: unification merges mutability equivalence classes
   (order-independent, alias-preserving); instantiating a generalized row
   duplicates each class once per instance; structure-generalized copies
   share classes; promotion and links are backtrackable; saving preserves
   class sharing without persisting links. *)

let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let assert_bool = OUnit.assert_bool

let int_typ () = Predef.type_int
let immutable_cell () = ref (Types.Mutability_value Asttypes.Immutable)
let mutable_cell () = ref (Types.Mutability_value Asttypes.Mutable)

(* An object row [{"x": int, ..}] whose field uses the given mutability
   cell; open (fresh row var) unless [closed]. *)
let obj_with_cell ?(closed = false) cell =
  let rest = if closed then Ctype.newty Types.Tnil else Ctype.newvar () in
  Ctype.newobj
    (Ctype.newty
       (Types.Tfield {name = "x"; mutability = cell; typ = int_typ (); rest}))

let field_of ty =
  match Ctype.flatten_fields (Ctype.object_fields ty) with
  | [f], _ -> f
  | _ -> OUnit.assert_failure "expected exactly one field"

let flag_of ty = Btype.mutability_repr (field_of ty).Ctype.f_mut

(* Class identity: the terminal cell of the link chain. *)
let cell_of ty = Btype.mutability_ref_repr (field_of ty).Ctype.f_mut

let write_through ty =
  (* Simulate [ty["x"] = v]: the promotion path used by assignment. *)
  match Ctype.filter_object_field_for_write Env.empty "x" ty with
  | Ok _ -> ()
  | Error _ -> OUnit.assert_failure "write lookup unexpectedly failed"

(* Two views sharing one cell (an alias group), a third object with its own
   cell. Returns (group_member_a, group_member_b, other). *)
let alias_setup () =
  let group_cell = immutable_cell () in
  let a = obj_with_cell group_cell in
  let b = obj_with_cell group_cell in
  let other = obj_with_cell (immutable_cell ()) in
  (a, b, other)

let test_unification_merges_alias_groups _ =
  let a, b, other = alias_setup () in
  Ctype.unify Env.empty other a;
  write_through other;
  assert_bool "promotion through the unified view reaches the direct alias"
    (flag_of a = Asttypes.Mutable);
  assert_bool
    "promotion also reaches the other member of the merged class: unification \
     merges groups, it does not splice single nodes"
    (flag_of b = Asttypes.Mutable)

let test_unification_order_is_irrelevant _ =
  let a1, b1, other1 = alias_setup () in
  Ctype.unify Env.empty other1 a1;
  write_through other1;
  ignore a1;
  let promoted_order1 = flag_of b1 = Asttypes.Mutable in
  let a2, b2, other2 = alias_setup () in
  Ctype.unify Env.empty a2 other2;
  write_through other2;
  ignore a2;
  let promoted_order2 = flag_of b2 = Asttypes.Mutable in
  assert_bool "the merged class sees the promotion in either argument order"
    (promoted_order1 && promoted_order2)

let test_backtracking_restores_promotion_and_links _ =
  let source = obj_with_cell (immutable_cell ()) in
  let target = obj_with_cell ~closed:true (mutable_cell ()) in
  let source_cell_before = cell_of source in
  let target_cell_before = cell_of target in
  let snap = Btype.snapshot () in
  (* Immutable+open vs Mutable: promotes the source class, then merges the
     classes. *)
  Ctype.unify Env.empty source target;
  assert_bool "promotion happened inside the trial"
    (flag_of source = Asttypes.Mutable);
  assert_bool "the classes are merged inside the trial"
    (cell_of source == cell_of target);
  Btype.backtrack snap;
  assert_bool "backtracking undoes the promotion"
    (flag_of source = Asttypes.Immutable);
  assert_bool "backtracking undoes the class merge"
    (cell_of target != cell_of source);
  assert_bool "the separated classes keep their own values"
    (cell_of source == source_cell_before
    && cell_of target == target_cell_before
    && flag_of target = Asttypes.Mutable)

let test_generalized_instances_are_independent _ =
  (* A let-polymorphic scheme (generic row terminator): each instance must
     promote independently, and never the scheme. *)
  Ctype.begin_def ();
  let scheme = obj_with_cell (immutable_cell ()) in
  Ctype.end_def ();
  Ctype.generalize scheme;
  let inst1 = Ctype.instance Env.empty scheme in
  let inst2 = Ctype.instance Env.empty scheme in
  write_through inst1;
  assert_bool "promoted instance is Mutable" (flag_of inst1 = Asttypes.Mutable);
  assert_bool "sibling instance stays Immutable"
    (flag_of inst2 = Asttypes.Immutable);
  assert_bool "the scheme itself stays Immutable"
    (flag_of scheme = Asttypes.Immutable)

let test_generalized_instance_preserves_internal_aliasing _ =
  (* Two fields of one scheme sharing a class: one instantiation must give
     both fields ONE fresh class (aliases stay correlated inside the
     instance), not one class each. *)
  Ctype.begin_def ();
  let cell = immutable_cell () in
  let a = obj_with_cell cell in
  let b = obj_with_cell cell in
  let pair = Ctype.newty (Types.Ttuple [a; b]) in
  Ctype.end_def ();
  Ctype.generalize pair;
  let inst = Ctype.instance Env.empty pair in
  let a', b' =
    match (Btype.repr inst).desc with
    | Types.Ttuple [a'; b'] -> (a', b')
    | _ -> OUnit.assert_failure "expected an instantiated pair"
  in
  assert_bool "the instance's aliases share one fresh class"
    (cell_of a' == cell_of b');
  assert_bool "the fresh class is not the scheme's"
    (cell_of a' != Btype.mutability_ref_repr cell)

let test_structure_generalized_occurrences_share _ =
  (* A parameter-annotation-like type: structure is generalized but the row
     terminator stays at the current level, so occurrences (instances) share
     the class and a promotion is visible through all of them. *)
  let annotated = obj_with_cell (immutable_cell ()) in
  Ctype.generalize_structure annotated;
  let occurrence1 = Ctype.instance Env.empty annotated in
  let occurrence2 = Ctype.instance Env.empty annotated in
  write_through occurrence1;
  assert_bool "promotion is visible through the other occurrence"
    (flag_of occurrence2 = Asttypes.Mutable);
  assert_bool "promotion is visible through the annotation itself"
    (flag_of annotated = Asttypes.Mutable)

(* Copy-policy coverage for ordinary typing and substitution paths. *)

let terminator_of ty =
  let _, rest = Ctype.flatten_fields (Ctype.object_fields ty) in
  Btype.repr rest

let abstract_type_decl type_manifest : Types.type_declaration =
  {
    type_params = [];
    type_arity = 0;
    type_kind = Type_abstract;
    type_private = Public;
    type_manifest;
    type_variance = [];
    type_newtype_level = None;
    type_loc = Location.none;
    type_attributes = [];
    type_immediate = false;
    type_representation = Boxed;
    type_inlined_types = [];
  }

let test_unified_owners_share_terminator _ =
  let a = obj_with_cell (immutable_cell ()) in
  let b = obj_with_cell ~closed:true (immutable_cell ()) in
  Ctype.unify Env.empty a b;
  assert_bool "unification makes both rows end at the same terminator node"
    (terminator_of a == terminator_of b)

let test_shared_copy_shares_terminator _ =
  let annotated = obj_with_cell (immutable_cell ()) in
  Ctype.generalize_structure annotated;
  let occurrence = Ctype.instance Env.empty annotated in
  assert_bool "class shared" (cell_of occurrence == cell_of annotated);
  assert_bool "terminator shared"
    (terminator_of occurrence == terminator_of annotated)

let test_generalized_instance_fresh_cell_fresh_terminator _ =
  Ctype.begin_def ();
  let scheme = obj_with_cell (immutable_cell ()) in
  Ctype.end_def ();
  Ctype.generalize scheme;
  let inst = Ctype.instance Env.empty scheme in
  assert_bool "class fresh" (cell_of inst != cell_of scheme);
  assert_bool "terminator fresh" (terminator_of inst != terminator_of scheme)

let test_subst_generic_copy_gets_fresh_cell _ =
  Ctype.begin_def ();
  let scheme = obj_with_cell (immutable_cell ()) in
  Ctype.end_def ();
  Ctype.generalize scheme;
  let copy = Subst.type_expr Subst.identity scheme in
  assert_bool "subst of a generic row refreshes the class"
    (cell_of copy != cell_of scheme)

let test_nondep_type_ends_its_copy_session _ =
  (* [nondep_type] copies through [copy_type_desc], whose cell duplication
     temporarily links originals to their duplicates; the nondep entry
     points must end that session.
     A leaked session would leave the scheme's representative linked and
     let a later unrelated copy session restore stale values. *)
  Ctype.begin_def ();
  let scheme = obj_with_cell (immutable_cell ()) in
  Ctype.end_def ();
  Ctype.generalize scheme;
  let copy = Ctype.nondep_type Env.empty (Ident.create "M") scheme in
  assert_bool "after nondep_type the scheme's cell is a direct value again"
    (match !((field_of scheme).Ctype.f_mut) with
    | Types.Mutability_value _ -> true
    | Types.Mutability_link _ -> false);
  assert_bool "the nondep copy has its own class"
    (cell_of copy != cell_of scheme);
  let source_inst = Ctype.instance Env.empty scheme in
  write_through source_inst;
  let copy_inst = Ctype.instance Env.empty copy in
  assert_bool "promoting a source instance does not reach the nondep copy"
    (flag_of copy_inst = Asttypes.Immutable && flag_of copy = Asttypes.Immutable);
  write_through copy_inst;
  ignore (Ctype.instance Env.empty scheme);
  assert_bool "a subsequent unrelated copy session undoes no promotion"
    (flag_of source_inst = Asttypes.Mutable
    && flag_of copy_inst = Asttypes.Mutable
    && flag_of scheme = Asttypes.Immutable
    && flag_of copy = Asttypes.Immutable)

let test_nondep_nested_copy_preserves_class_sharing _ =
  let alias_id = Ident.create "m" in
  let alias_decl = abstract_type_decl (Some Predef.type_int) in
  let env = Env.add_type ~check:false alias_id alias_decl Env.empty in
  Ctype.begin_def ();
  let cell = immutable_cell () in
  let first = obj_with_cell cell in
  let dependent_alias =
    Ctype.newty (Types.Tconstr (Path.Pident alias_id, [], ref Types.Mnil))
  in
  let second = obj_with_cell cell in
  let scheme = Ctype.newty (Types.Ttuple [first; dependent_alias; second]) in
  Ctype.end_def ();
  Ctype.generalize scheme;
  let copy = Ctype.nondep_type env alias_id scheme in
  let first', second' =
    match (Btype.repr copy).desc with
    | Types.Ttuple [first'; _expanded_alias; second'] -> (first', second')
    | _ -> OUnit.assert_failure "expected a copied triple"
  in
  assert_bool
    "nested abbreviation instantiation does not end the outer copy session"
    (cell_of first' == cell_of second')

let test_nondep_failure_ends_copy_session _ =
  let alias_id = Ident.create "m" in
  let env =
    Env.add_type ~check:false alias_id (abstract_type_decl None) Env.empty
  in
  Ctype.begin_def ();
  let cell = immutable_cell () in
  let first = obj_with_cell cell in
  let dependent_alias =
    Ctype.newty (Types.Tconstr (Path.Pident alias_id, [], ref Types.Mnil))
  in
  let scheme = Ctype.newty (Types.Ttuple [first; dependent_alias]) in
  Ctype.end_def ();
  Ctype.generalize scheme;
  assert_bool "the dependent abstract type cannot be removed"
    (match Ctype.nondep_type env alias_id scheme with
    | _ -> false
    | exception Not_found -> true);
  assert_bool "the failed copy restored its temporary mutability link"
    (match !((field_of first).Ctype.f_mut) with
    | Types.Mutability_value Asttypes.Immutable -> true
    | Types.Mutability_value Asttypes.Mutable | Types.Mutability_link _ -> false)

let test_saving_closed_row_gets_fresh_resolved_cell _ =
  (* A saved graph owns fresh cells even for closed rows. The copied cell must
     contain the resolved value: saved graphs never contain [Mutability_link],
     even when the source field's own ref is a merged-class link. *)
  let rep = immutable_cell () in
  let a = obj_with_cell ~closed:true (ref (Types.Mutability_link rep)) in
  assert_bool "the source field holds a link (a merged class member)"
    (match !((field_of a).Ctype.f_mut) with
    | Types.Mutability_link _ -> true
    | Types.Mutability_value _ -> false);
  let saved = Subst.type_expr (Subst.for_saving Subst.identity) a in
  assert_bool "the saved field holds a value cell, not a link"
    (match !((field_of saved).Ctype.f_mut) with
    | Types.Mutability_value _ -> true
    | Types.Mutability_link _ -> false);
  assert_bool "the saved flag is the class value"
    (flag_of saved = Asttypes.Immutable);
  assert_bool "the saved graph does not retain the source cell"
    (cell_of saved != cell_of a)

let test_saving_marshal_round_trip _ =
  (* The real persistence claim: after [for_saving] the graph marshals, and
     unmarshalling preserves both the flags and the sharing relation. *)
  let cell = immutable_cell () in
  Ctype.begin_def ();
  let pair =
    Ctype.newty (Types.Ttuple [obj_with_cell cell; obj_with_cell cell])
  in
  Ctype.end_def ();
  Ctype.generalize pair;
  let saved = Subst.type_expr (Subst.for_saving Subst.identity) pair in
  let reloaded : Types.type_expr =
    Marshal.from_string (Marshal.to_string saved []) 0
  in
  let a', b' =
    match (Btype.repr reloaded).desc with
    | Types.Ttuple [a'; b'] -> (a', b')
    | _ -> OUnit.assert_failure "expected a reloaded pair"
  in
  assert_bool "reloaded flags are values with the saved state"
    (flag_of a' = Asttypes.Immutable && flag_of b' = Asttypes.Immutable);
  assert_bool "reloaded spines still share one cell" (cell_of a' == cell_of b')

let test_saving_preserves_class_sharing _ =
  (* R6: saving removes links but keeps the sharing relation — two spines
     sharing one class still share one (fresh, link-free) cell after
     [for_saving]. *)
  let cell = immutable_cell () in
  Ctype.begin_def ();
  let a = obj_with_cell cell in
  let b = obj_with_cell cell in
  let pair = Ctype.newty (Types.Ttuple [a; b]) in
  Ctype.end_def ();
  Ctype.generalize pair;
  let saved = Subst.type_expr (Subst.for_saving Subst.identity) pair in
  let a', b' =
    match (Btype.repr saved).desc with
    | Types.Ttuple [a'; b'] -> (a', b')
    | _ -> OUnit.assert_failure "expected a saved pair"
  in
  assert_bool "saved spines still share one mutability cell"
    (cell_of a' == cell_of b');
  assert_bool "the saved cell holds a value, not a link"
    (match !((field_of a').Ctype.f_mut) with
    | Types.Mutability_value _ -> true
    | Types.Mutability_link _ -> false)

let test_for_saving_copy_order_is_irrelevant _ =
  (* [generalize_structure] makes the field spine generic while leaving its
     open-row terminator non-generic. [for_saving] makes the copied terminator
     generic. If the two graphs still share one mutability cell, copying them
     in one session can then choose different policies for that cell. *)
  Ctype.begin_def ();
  let source = obj_with_cell (immutable_cell ()) in
  Ctype.end_def ();
  Ctype.generalize_structure source;
  let saved = Subst.type_expr (Subst.for_saving Subst.identity) source in
  assert_bool "the fixture has different row-copy classifications"
    ((terminator_of source).level <> Btype.generic_level
    && (terminator_of saved).level = Btype.generic_level);
  assert_bool "for_saving gives the copied graph an independent class"
    (cell_of source != cell_of saved);
  let copy_pair first second =
    match Ctype.instance_list Env.empty [first; second] with
    | [first'; second'] -> (first', second')
    | _ -> OUnit.assert_failure "expected two copied object types"
  in
  let source_first, saved_second = copy_pair source saved in
  let saved_first, source_second = copy_pair saved source in
  let source_first_shares = cell_of source_first == cell_of saved_second in
  let saved_first_shares = cell_of saved_first == cell_of source_second in
  assert_bool "source-first copies are separate" (not source_first_shares);
  assert_bool "saved-first copies are separate" (not saved_first_shares);
  assert_bool
    "copying owners of one class must not depend on their order in the session"
    (source_first_shares = saved_first_shares)

let private_row_obj cell =
  let rest =
    Ctype.newty
      (Types.Tconstr (Path.Pident (Ident.create "t#row"), [], ref Types.Mnil))
  in
  Ctype.newobj
    (Ctype.newty
       (Types.Tfield {name = "x"; mutability = cell; typ = int_typ (); rest}))

let test_private_row_write_does_not_promote _ =
  (* Structural openness includes a [Tconstr] terminator, but assignment must
     use the same [Tvar] gate as [unify_mutability]: copies share this cell
     with the declaration. *)
  let cell = immutable_cell () in
  let ty = private_row_obj cell in
  assert_bool "the private row is structurally open"
    (Ctype.object_row_is_structurally_open ty);
  (match Ctype.filter_object_field_for_write Env.empty "x" ty with
  | Error Ctype.Owrite_not_mutable -> ()
  | Error Ctype.Owrite_missing ->
    OUnit.assert_failure "expected not-mutable, got missing"
  | Ok _ -> OUnit.assert_failure "private-row write should be rejected");
  assert_bool "the declaration cell is untouched"
    (Btype.mutability_repr cell = Asttypes.Immutable)

let test_for_saving_fresh_copy_preserves_internal_aliasing _ =
  Ctype.begin_def ();
  let cell = immutable_cell () in
  let source =
    Ctype.newty (Types.Ttuple [obj_with_cell cell; obj_with_cell cell])
  in
  Ctype.end_def ();
  Ctype.generalize_structure source;
  let saved = Subst.type_expr (Subst.for_saving Subst.identity) source in
  let source_a, source_b, saved_a, saved_b =
    match ((Btype.repr source).desc, (Btype.repr saved).desc) with
    | Types.Ttuple [source_a; source_b], Types.Ttuple [saved_a; saved_b] ->
      (source_a, source_b, saved_a, saved_b)
    | _ -> OUnit.assert_failure "expected source and saved object pairs"
  in
  assert_bool "the source aliases share one class"
    (cell_of source_a == cell_of source_b);
  assert_bool "the saved aliases share one fresh class"
    (cell_of saved_a == cell_of saved_b && cell_of saved_a != cell_of source_a)

let suites =
  __FILE__
  >::: [
         "unification_merges_alias_groups"
         >:: test_unification_merges_alias_groups;
         "unification_order_is_irrelevant"
         >:: test_unification_order_is_irrelevant;
         "backtracking_restores_promotion_and_links"
         >:: test_backtracking_restores_promotion_and_links;
         "generalized_instances_are_independent"
         >:: test_generalized_instances_are_independent;
         "generalized_instance_preserves_internal_aliasing"
         >:: test_generalized_instance_preserves_internal_aliasing;
         "structure_generalized_occurrences_share"
         >:: test_structure_generalized_occurrences_share;
         "unified_owners_share_terminator"
         >:: test_unified_owners_share_terminator;
         "shared_copy_shares_terminator" >:: test_shared_copy_shares_terminator;
         "generalized_instance_fresh_cell_fresh_terminator"
         >:: test_generalized_instance_fresh_cell_fresh_terminator;
         "subst_generic_copy_gets_fresh_cell"
         >:: test_subst_generic_copy_gets_fresh_cell;
         "nondep_type_ends_its_copy_session"
         >:: test_nondep_type_ends_its_copy_session;
         "nondep_nested_copy_preserves_class_sharing"
         >:: test_nondep_nested_copy_preserves_class_sharing;
         "nondep_failure_ends_copy_session"
         >:: test_nondep_failure_ends_copy_session;
         "saving_closed_row_gets_fresh_resolved_cell"
         >:: test_saving_closed_row_gets_fresh_resolved_cell;
         "saving_marshal_round_trip" >:: test_saving_marshal_round_trip;
         "saving_preserves_class_sharing"
         >:: test_saving_preserves_class_sharing;
         "for_saving_copy_order_is_irrelevant"
         >:: test_for_saving_copy_order_is_irrelevant;
         "for_saving_fresh_copy_preserves_internal_aliasing"
         >:: test_for_saving_fresh_copy_preserves_internal_aliasing;
         "private_row_write_does_not_promote"
         >:: test_private_row_write_does_not_promote;
       ]

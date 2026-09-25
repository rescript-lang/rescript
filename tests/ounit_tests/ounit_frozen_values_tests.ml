let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let assert_bool = OUnit.assert_bool

let abstract_type : Types.type_declaration =
  {
    type_params = [];
    type_arity = 0;
    type_kind = Type_abstract;
    type_private = Public;
    type_manifest = None;
    type_variance = [];
    type_newtype_level = None;
    type_loc = Location.none;
    type_attributes = [];
    type_immediate = false;
    type_representation = Boxed;
    type_inlined_types = [];
  }

let value ?(attributes = []) ty : Types.value_description =
  {
    val_type = ty;
    val_kind = Val_reg;
    val_loc = Location.none;
    val_attributes = attributes;
  }

let freeze signature =
  let cmi : Cmi_format.cmi_infos =
    {cmi_name = "Api"; cmi_sign = signature; cmi_crcs = []; cmi_flags = []}
  in
  match Frozen_values.freeze cmi with
  | Ok image -> image
  | Error reason -> OUnit.assert_failure reason

let test_prefixed_type_identity_and_positions _ =
  let type_id = Ident.create "t" in
  let value_type =
    Btype.newgenty (Types.Tconstr (Path.Pident type_id, [], ref Types.Mnil))
  in
  let image =
    freeze
      [
        Types.Sig_type (type_id, abstract_type, Types.Trec_not);
        Types.Sig_value (Ident.create "first", value value_type);
        Types.Sig_module
          ( Ident.create "Nested",
            {
              Types.md_type = Mty_signature [];
              md_attributes = [];
              md_loc = Location.none;
            },
            Types.Trec_not );
        Types.Sig_value (Ident.create "second", value value_type);
      ]
  in
  OUnit.assert_equal 2 (Frozen_values.value_count image);
  let view = Frozen_values.create_view image in
  match (Frozen_values.find view "first", Frozen_values.find view "second") with
  | Some (first, first_pos), Some (second, second_pos) -> (
    OUnit.assert_equal 0 first_pos;
    OUnit.assert_equal 2 second_pos;
    assert_bool "one request view preserves value type sharing"
      (first.val_type == second.val_type);
    (match Frozen_values.find view "first" with
    | Some (again, _) ->
      assert_bool "one view reuses its materialized declaration" (again == first)
    | None -> OUnit.assert_failure "expected first value again");
    match first.val_type.desc with
    | Types.Tconstr (path, [], _) ->
      let expected =
        Path.Pdot (Path.Pident (Ident.create_persistent "Api"), "t", Path.nopos)
      in
      assert_bool "local type identity is prefixed by the importing module"
        (Path.same path expected)
    | _ -> OUnit.assert_failure "expected a named type")
  | _ -> OUnit.assert_failure "expected two exported values"

let test_views_are_independent _ =
  let source = Btype.newgenty (Types.Tvar (Some "a")) in
  let image = freeze [Types.Sig_value (Ident.create "value", value source)] in
  let get () =
    match Frozen_values.find (Frozen_values.create_view image) "value" with
    | Some (description, _) -> description.val_type
    | None -> OUnit.assert_failure "missing value"
  in
  let first = Domain.spawn get in
  let second = Domain.spawn get in
  let first = Domain.join first in
  let second = Domain.join second in
  assert_bool "domain views own their type nodes" (first != second);
  first.desc <- Types.Tvar (Some "changed");
  match (second.desc, source.desc) with
  | Types.Tvar (Some "a"), Types.Tvar (Some "a") -> ()
  | _ -> OUnit.assert_failure "a view changed another graph"

let test_attributes_are_copied _ =
  let attributes = [(Location.mknoloc "tag", Parsetree.PStr [])] in
  let image =
    freeze
      [
        Types.Sig_value
          ( Ident.create "value",
            value ~attributes (Btype.newgenty (Types.Tvar None)) );
      ]
  in
  let get () =
    match Frozen_values.find (Frozen_values.create_view image) "value" with
    | Some (description, _) -> description.val_attributes
    | None -> OUnit.assert_failure "missing value"
  in
  let first = get () in
  let second = get () in
  assert_bool "metadata does not expose the source or another view"
    (first != attributes && first != second);
  OUnit.assert_equal attributes first;
  OUnit.assert_equal attributes second

let test_abstract_type_manifest_is_local_and_prefixed _ =
  let other_id = Ident.create "other" in
  let type_id = Ident.create "t" in
  let parameter = Btype.newgenty (Types.Tvar (Some "a")) in
  let manifest =
    Btype.newgenty
      (Types.Tconstr (Path.Pident other_id, [parameter], ref Types.Mnil))
  in
  let declaration =
    {
      abstract_type with
      type_params = [parameter];
      type_arity = 1;
      type_manifest = Some manifest;
    }
  in
  let image =
    freeze
      [
        Types.Sig_type (other_id, abstract_type, Types.Trec_not);
        Types.Sig_type (type_id, declaration, Types.Trec_not);
      ]
  in
  OUnit.assert_equal 2 (Frozen_values.type_count image);
  let first_view = Frozen_values.create_view image in
  let second_view = Frozen_values.create_view image in
  match
    ( Frozen_values.find_type first_view "t",
      Frozen_values.find_type second_view "t" )
  with
  | Some (first, _), Some (second, _) -> (
    assert_bool "type declaration is cached within one view"
      (match Frozen_values.find_type first_view "t" with
      | Some (again, _) -> again == first
      | None -> false);
    assert_bool "type parameters are independent across views"
      (List.hd first.type_params != List.hd second.type_params);
    match first.type_manifest with
    | Some {desc = Types.Tconstr (path, [argument], _)} ->
      let expected =
        Path.Pdot
          (Path.Pident (Ident.create_persistent "Api"), "other", Path.nopos)
      in
      assert_bool "manifest path is prefixed" (Path.same path expected);
      assert_bool "manifest shares its parameter within a view"
        (argument == List.hd first.type_params)
    | _ -> OUnit.assert_failure "expected a parameterized manifest")
  | _ -> OUnit.assert_failure "expected abstract type declarations"

let test_record_labels_are_local _ =
  let field_type = Btype.newgenty (Types.Tvar None) in
  let label : Types.label_declaration =
    {
      ld_id = Ident.create "value";
      ld_runtime_name = None;
      ld_mutable = Immutable;
      ld_optional = false;
      ld_type = field_type;
      ld_loc = Location.none;
      ld_attributes = [];
    }
  in
  let image =
    freeze
      [
        Types.Sig_type
          ( Ident.create "box",
            {
              abstract_type with
              type_kind = Type_record ([label], Record_regular);
            },
            Types.Trec_not );
      ]
  in
  let first = Frozen_values.create_view image in
  let second = Frozen_values.create_view image in
  let identifier_time = Ident.current_time () in
  match
    ( Frozen_values.find_labels first "value",
      Frozen_values.find_labels second "value" )
  with
  | Some [first_label], Some [second_label] -> (
    OUnit.assert_equal identifier_time (Ident.current_time ());
    assert_bool "label descriptors are request-local"
      (first_label != second_label
      && first_label.lbl_arg != second_label.lbl_arg
      && first_label.lbl_all != second_label.lbl_all);
    (match
       ( Frozen_values.find_type first "box",
         Frozen_values.find_type second "box" )
     with
    | ( Some ({type_kind = Types.Type_record ([left], _)}, _),
        Some ({type_kind = Types.Type_record ([right], _)}, _) ) ->
      assert_bool "label identifiers receive fresh stamps"
        (not (Ident.same left.ld_id right.ld_id))
    | _ -> OUnit.assert_failure "expected two record declarations");
    assert_bool "one view reuses its label descriptor"
      (match Frozen_values.find_labels first "value" with
      | Some [again] -> again == first_label
      | _ -> false);
    match Frozen_values.find_type first "box" with
    | Some (declaration, (_, [type_label])) -> (
      assert_bool "label lookup and type lookup share one description"
        (first_label == type_label);
      match declaration.type_kind with
      | Types.Type_record ([materialized], Types.Record_regular) ->
        assert_bool "label type belongs to the same view"
          (materialized.ld_type == first_label.lbl_arg)
      | _ -> OUnit.assert_failure "expected record type")
    | _ -> OUnit.assert_failure "expected a record declaration")
  | _ -> OUnit.assert_failure "expected record labels"

let test_shadowed_record_labels_fall_back _ =
  let field_type = Btype.newgenty (Types.Tvar None) in
  let label : Types.label_declaration =
    {
      ld_id = Ident.create "value";
      ld_runtime_name = None;
      ld_mutable = Immutable;
      ld_optional = false;
      ld_type = field_type;
      ld_loc = Location.none;
      ld_attributes = [];
    }
  in
  let record =
    {abstract_type with type_kind = Type_record ([label], Record_regular)}
  in
  let image =
    freeze
      [
        Types.Sig_type (Ident.create "box", record, Types.Trec_not);
        Types.Sig_type (Ident.create "box", record, Types.Trec_not);
      ]
  in
  match Frozen_values.find_labels (Frozen_values.create_view image) "value" with
  | None -> ()
  | Some _ -> OUnit.assert_failure "shadowed records need component lookup"

let test_variant_constructors_are_local _ =
  let constructor name args : Types.constructor_declaration =
    {
      cd_id = Ident.create name;
      cd_runtime_tag = None;
      cd_args = Cstr_tuple args;
      cd_res = None;
      cd_loc = Location.none;
      cd_attributes = [];
    }
  in
  let argument = Btype.newgenty (Types.Tvar None) in
  let layout = Variant_runtime.plain_layout [("A", false); ("B", true)] in
  let variant =
    {
      abstract_type with
      type_kind =
        Type_variant ([constructor "A" []; constructor "B" [argument]], layout);
    }
  in
  let image =
    freeze [Types.Sig_type (Ident.create "choice", variant, Types.Trec_not)]
  in
  let first = Frozen_values.create_view image in
  let second = Frozen_values.create_view image in
  let identifier_time = Ident.current_time () in
  match
    ( Frozen_values.find_constructors first "B",
      Frozen_values.find_constructors second "B" )
  with
  | Some [left], Some [right] -> (
    OUnit.assert_equal identifier_time (Ident.current_time ());
    assert_bool "constructor descriptors and types are request-local"
      (left != right && List.hd left.cstr_args != List.hd right.cstr_args);
    (match (left.cstr_kind, right.cstr_kind) with
    | Types.Ordinary_constructor left_ref, Types.Ordinary_constructor right_ref
      ->
      assert_bool "runtime layouts are request-local"
        (left_ref.variant != right_ref.variant)
    | _ -> OUnit.assert_failure "expected ordinary constructors");
    match Frozen_values.find_type first "choice" with
    | Some (_, ([first_a; first_b], _)) ->
      assert_bool "constructor lookup reuses the type description"
        (first_b == left && first_a.cstr_name = "A")
    | _ -> OUnit.assert_failure "expected two constructors")
  | _ -> OUnit.assert_failure "expected constructor B"

let test_extension_constructors_are_local _ =
  let payload = Btype.newgenty (Types.Tvar None) in
  let extension : Types.extension_constructor =
    {
      ext_type_path = Predef.path_exn;
      ext_type_params = [];
      ext_args = Cstr_tuple [payload];
      ext_ret_type = None;
      ext_private = Public;
      ext_loc = Location.none;
      ext_attributes = [];
      ext_is_exception = true;
    }
  in
  let image =
    freeze [Types.Sig_typext (Ident.create "Boom", extension, Text_exception)]
  in
  let first = Frozen_values.create_view image in
  let second = Frozen_values.create_view image in
  let identifier_time = Ident.current_time () in
  match
    ( Frozen_values.find_extension first "Boom",
      Frozen_values.find_constructors first "Boom",
      Frozen_values.find_extension second "Boom" )
  with
  | Some left, Some [again], Some right -> (
    OUnit.assert_equal identifier_time (Ident.current_time ());
    assert_bool "extension lookup reuses the constructor" (left == again);
    assert_bool "extension payloads belong to each request"
      (List.hd left.cstr_args != List.hd right.cstr_args);
    match left.cstr_kind with
    | Types.Extension_constructor path ->
      assert_bool "extension path includes its signature position"
        (Path.same path
           (Path.Pdot (Path.Pident (Ident.create_persistent "Api"), "Boom", 0)))
    | _ -> OUnit.assert_failure "expected an extension constructor")
  | _ -> OUnit.assert_failure "expected a frozen extension constructor"

let test_nested_scope_paths_and_sharing _ =
  let outer_id = Ident.create "outer" in
  let inner_id = Ident.create "inner" in
  let inner_type =
    Btype.newgenty (Types.Tconstr (Path.Pident inner_id, [], ref Types.Mnil))
  in
  let outer_type =
    Btype.newgenty (Types.Tconstr (Path.Pident outer_id, [], ref Types.Mnil))
  in
  let module_decl signature : Types.module_declaration =
    {
      md_type = Mty_signature signature;
      md_attributes = [];
      md_loc = Location.none;
    }
  in
  let image =
    freeze
      [
        Types.Sig_type (outer_id, abstract_type, Trec_not);
        Types.Sig_module
          ( Ident.create "Nested",
            module_decl
              [
                Types.Sig_type
                  ( inner_id,
                    {abstract_type with type_manifest = Some outer_type},
                    Trec_not );
                Types.Sig_value (Ident.create "value", value inner_type);
                Types.Sig_module
                  ( Ident.create "Deep",
                    module_decl
                      [Types.Sig_value (Ident.create "value", value inner_type)],
                    Trec_not );
              ],
            Trec_not );
      ]
  in
  let view = Frozen_values.create_view image in
  match Frozen_values.find_module (Frozen_values.root_scope view) "Nested" with
  | Some (nested, nested_pos, _, _) -> (
    OUnit.assert_equal 0 nested_pos;
    match
      ( Frozen_values.find_type_in_scope view nested "inner",
        Frozen_values.find_in_scope view nested "value",
        Frozen_values.find_module nested "Deep" )
    with
    | Some (declaration, _), Some (value, value_pos), Some (deep, deep_pos, _, _)
      -> (
      OUnit.assert_equal 0 value_pos;
      OUnit.assert_equal 1 deep_pos;
      (match declaration.type_manifest with
      | Some {desc = Types.Tconstr (path, _, _)} ->
        assert_bool "nested manifest resolves an outer binder"
          (Path.same path
             (Path.Pdot
                ( Path.Pident (Ident.create_persistent "Api"),
                  "outer",
                  Path.nopos )))
      | _ -> OUnit.assert_failure "expected an outer type manifest");
      match
        (value.val_type.desc, Frozen_values.find_in_scope view deep "value")
      with
      | Types.Tconstr (path, _, _), Some (deep_value, 0) ->
        assert_bool "nested binder has a qualified path"
          (Path.same path
             (Path.Pdot
                ( Path.Pdot
                    (Path.Pident (Ident.create_persistent "Api"), "Nested", 0),
                  "inner",
                  Path.nopos )));
        assert_bool "one arena shares types across scopes"
          (value.val_type == deep_value.val_type)
      | _ -> OUnit.assert_failure "expected a deep value")
    | _ -> OUnit.assert_failure "expected nested declarations")
  | None -> OUnit.assert_failure "expected a nested frozen signature"

let test_reused_module_type_has_distinct_binders _ =
  let signature_id = Ident.create "S" in
  let type_id = Ident.create "t" in
  let value_type =
    Btype.newgenty (Types.Tconstr (Path.Pident type_id, [], ref Types.Mnil))
  in
  let signature =
    [
      Types.Sig_type (type_id, abstract_type, Trec_not);
      Types.Sig_value (Ident.create "value", value value_type);
    ]
  in
  let module_decl : Types.module_declaration =
    {
      md_type = Mty_ident (Path.Pident signature_id);
      md_attributes = [];
      md_loc = Location.none;
    }
  in
  let image =
    freeze
      [
        Types.Sig_modtype
          ( signature_id,
            {
              mtd_type = Some (Mty_signature signature);
              mtd_attributes = [];
              mtd_loc = Location.none;
            } );
        Types.Sig_module (Ident.create "A", module_decl, Trec_not);
        Types.Sig_module (Ident.create "B", module_decl, Trec_not);
      ]
  in
  let view = Frozen_values.create_view image in
  let root = Frozen_values.root_scope view in
  match
    (Frozen_values.find_module root "A", Frozen_values.find_module root "B")
  with
  | Some (a, 0, _, _), Some (b, 1, _, _) -> (
    match
      ( Frozen_values.find_in_scope view a "value",
        Frozen_values.find_in_scope view b "value",
        Frozen_values.find_module_declaration view root "A",
        Frozen_values.find_modtype_declaration view root "S" )
    with
    | ( Some (a_value, 0),
        Some (b_value, 0),
        Some ({md_type = Types.Mty_ident module_type_path}, 0),
        Some {mtd_type = Some (Types.Mty_signature _)} ) -> (
      assert_bool "reuse gets independent mutable type graphs"
        (a_value.val_type != b_value.val_type);
      assert_bool "module type path is prefixed"
        (Path.same module_type_path
           (Path.Pdot
              (Path.Pident (Ident.create_persistent "Api"), "S", Path.nopos)));
      match (a_value.val_type.desc, b_value.val_type.desc) with
      | Types.Tconstr (a_path, _, _), Types.Tconstr (b_path, _, _) ->
        assert_bool "A uses its own abstract type"
          (Path.same a_path
             (Path.Pdot
                ( Path.Pdot (Path.Pident (Ident.create_persistent "Api"), "A", 0),
                  "t",
                  Path.nopos )));
        assert_bool "B uses its own abstract type"
          (Path.same b_path
             (Path.Pdot
                ( Path.Pdot (Path.Pident (Ident.create_persistent "Api"), "B", 1),
                  "t",
                  Path.nopos )))
      | _ -> OUnit.assert_failure "expected two abstract type paths")
    | _ -> OUnit.assert_failure "expected reusable module type entries")
  | _ -> OUnit.assert_failure "expected two module type instantiations"

let test_open_and_inline_record_types _ =
  let label : Types.label_declaration =
    {
      ld_id = Ident.create "field";
      ld_runtime_name = None;
      ld_mutable = Immutable;
      ld_optional = false;
      ld_type = Predef.type_int ();
      ld_loc = Location.none;
      ld_attributes = [];
    }
  in
  let layout = Variant_runtime.plain_layout [("Case", true)] in
  let constructor : Types.constructor_declaration =
    {
      cd_id = Ident.create "Case";
      cd_runtime_tag = None;
      cd_args = Cstr_record [label];
      cd_res = None;
      cd_loc = Location.none;
      cd_attributes = [];
    }
  in
  let inline_types = [Types.Record {type_name = "payload"; labels = [label]}] in
  let image =
    freeze
      [
        Types.Sig_type
          ( Ident.create "open_type",
            {abstract_type with type_kind = Type_open},
            Trec_not );
        Types.Sig_type
          ( Ident.create "choice",
            {
              abstract_type with
              type_kind = Type_variant ([constructor], layout);
              type_inlined_types = inline_types;
            },
            Trec_not );
        Types.Sig_type
          ( Ident.create "payload",
            {
              abstract_type with
              type_kind =
                Type_record
                  ( [label],
                    Record_inlined
                      {
                        name = "Case";
                        representation = {variant = layout; position = 0};
                      } );
              type_inlined_types = inline_types;
            },
            Trec_not );
      ]
  in
  let view = Frozen_values.create_view image in
  match
    ( Frozen_values.find_type view "open_type",
      Frozen_values.find_type view "choice",
      Frozen_values.find_type view "payload" )
  with
  | ( Some ({type_kind = Types.Type_open}, _),
      Some ({type_kind = Types.Type_variant (_, variant_layout)}, _),
      Some
        ( {
            type_kind =
              Types.Type_record
                (_, Types.Record_inlined {representation = inline_ref});
            type_inlined_types = [Types.Record {labels = [inline_label]}];
          },
          _ ) ) ->
    assert_bool "inline record shares its variant layout"
      (variant_layout == inline_ref.variant);
    assert_bool "inline metadata is request-owned"
      (inline_label.ld_id != label.ld_id)
  | _ -> OUnit.assert_failure "expected open and inline-record types"

let test_full_signature_copies_are_independent _ =
  let source = Btype.newgenty (Types.Tvar (Some "a")) in
  let image = freeze [Types.Sig_value (Ident.create "value", value source)] in
  let copy () =
    match Frozen_values.copy_signature (Frozen_values.create_view image) with
    | [Types.Sig_value (_, description)] -> description.val_type
    | _ -> OUnit.assert_failure "expected one copied value"
  in
  let first = copy () in
  let second = copy () in
  assert_bool "full signature copies have independent type graphs"
    (first != second);
  first.desc <- Types.Tvar (Some "changed");
  match (second.desc, source.desc) with
  | Types.Tvar (Some "a"), Types.Tvar (Some "a") -> ()
  | _ -> OUnit.assert_failure "a full signature copy leaked into another"

let suites =
  __FILE__
  >::: [
         "prefixed_type_identity_and_positions"
         >:: test_prefixed_type_identity_and_positions;
         "views_are_independent" >:: test_views_are_independent;
         "attributes_are_copied" >:: test_attributes_are_copied;
         "abstract_type_manifest_is_local_and_prefixed"
         >:: test_abstract_type_manifest_is_local_and_prefixed;
         "record_labels_are_local" >:: test_record_labels_are_local;
         "shadowed_record_labels_fall_back"
         >:: test_shadowed_record_labels_fall_back;
         "variant_constructors_are_local"
         >:: test_variant_constructors_are_local;
         "extension_constructors_are_local"
         >:: test_extension_constructors_are_local;
         "nested_scope_paths_and_sharing"
         >:: test_nested_scope_paths_and_sharing;
         "reused_module_type_has_distinct_binders"
         >:: test_reused_module_type_has_distinct_binders;
         "open_and_inline_record_types" >:: test_open_and_inline_record_types;
         "full_signature_copies_are_independent"
         >:: test_full_signature_copies_are_independent;
       ]

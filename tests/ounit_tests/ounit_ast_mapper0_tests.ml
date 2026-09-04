let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let assert_failure = OUnit.assert_failure

let loc = Location.none

let located_string ?(loc = loc) txt = {Location.txt; loc}

let source_loc start_cnum end_cnum =
  {
    Location.loc_start = {Lexing.dummy_pos with pos_cnum = start_cnum};
    loc_end = {Lexing.dummy_pos with pos_cnum = end_cnum};
    loc_ghost = false;
  }

let attr name payload = ({Location.txt = name; loc}, payload)

let has_attr name attrs =
  List.exists (fun ({Location.txt}, _) -> txt = name) attrs

let record_pat0 attrs =
  Ast_helper0.Pat.record ~loc ~attrs
    [
      ( Location.mknoloc (Longident.Lident "name"),
        Ast_helper0.Pat.var ~loc (Location.mknoloc "name") );
    ]
    Asttypes.Open

let map_pat0 pat =
  Ast_mapper_from0.default_mapper.pat Ast_mapper_from0.default_mapper pat

let test_public_record_rest_attr_is_not_internal _ =
  let pat =
    map_pat0 (record_pat0 [attr "res.record_rest" (Parsetree0.PStr [])])
  in
  match pat.ppat_desc with
  | Parsetree.Ppat_record (_, _, None) ->
    OUnit.assert_bool "public res.record_rest attribute was not preserved"
      (has_attr "res.record_rest" pat.ppat_attributes)
  | Parsetree.Ppat_record (_, _, Some _) ->
    assert_failure "public res.record_rest attribute was decoded as record rest"
  | _ -> assert_failure "Expected a record pattern"

let test_malformed_internal_record_rest_attr_fails _ =
  OUnit.assert_raises (Failure "Malformed internal _res.record_rest attribute")
    (fun () ->
      ignore
        (map_pat0 (record_pat0 [attr "_res.record_rest" (Parsetree0.PStr [])])))

let test_record_rest_roundtrips_through_ast0 _ =
  let rest =
    Some
      {
        Parsetree.rest_loc = loc;
        rest_name = Location.mknoloc "rest";
        rest_type = None;
      }
  in
  let pat =
    Ast_helper.Pat.record ~loc ?rest
      [
        {
          Parsetree.lid = Location.mknoloc (Longident.Lident "name");
          x = Ast_helper.Pat.var ~loc (Location.mknoloc "name");
          opt = false;
        };
      ]
      Asttypes.Open
  in
  let pat0 =
    Ast_mapper_to0.default_mapper.pat Ast_mapper_to0.default_mapper pat
  in
  let pat = map_pat0 pat0 in
  match pat.ppat_desc with
  | Parsetree.Ppat_record
      (_, _, Some {rest_name = {txt = "rest"; _}; rest_type = None; _}) ->
    ()
  | _ -> assert_failure "Expected record rest after ast0 roundtrip"

(* The ast-mapping fixtures show that a field's [@as] survives the roundtrip.
   What they cannot show is the shape it travels in: an external ppx reads the
   frozen AST, so the runtime name has to reach it as an ordinary [@as]
   attribute and not under some name of the bridge's own choosing. *)
let test_field_runtime_name_reaches_ast0_as_an_attribute _ =
  let as_attr =
    attr "as"
      (Parsetree.PStr
         [
           Ast_helper.Str.eval
             (Ast_helper.Exp.constant
                (Pconst_string (String_literal.string_from_semantic "renamed")));
         ])
  in
  let field =
    Ast_helper.Type.field ~loc ~attrs:[as_attr] (located_string "a")
      (Ast_helper.Typ.constr ~loc (located_string (Longident.Lident "int")) [])
  in
  OUnit.assert_bool "Expected the @as attribute to become the field"
    (field.pld_runtime_name <> None);
  let field0 =
    Ast_mapper_to0.default_mapper.label_declaration
      Ast_mapper_to0.default_mapper field
  in
  OUnit.assert_bool "Expected a plain @as attribute on the ast0 wire"
    (has_attr "as" field0.pld_attributes)

(* A ppx reads the attribute list in order, so the one taken out has to go back
   where it was written rather than at the front. *)
(* Ppx output carries no source position, so every attribute ties. The rename
   goes back last among them, the order a ppx writing [@dead @as("x")] gave. *)
let test_field_runtime_name_keeps_ppx_order_on_the_wire _ =
  let field =
    Ast_helper.Type.field ~loc:Location.none
      ~attrs:
        [
          (Location.mknoloc "dead", Parsetree.PStr []);
          ( Location.mknoloc "as",
            Parsetree.PStr
              [
                Ast_helper.Str.eval
                  (Ast_helper.Exp.constant
                     (Pconst_string (String_literal.string_from_semantic "wire")));
              ] );
        ]
      (Location.mknoloc "a")
      (Ast_helper.Typ.constr (Location.mknoloc (Longident.Lident "int")) [])
  in
  let field0 =
    Ast_mapper_to0.default_mapper.label_declaration
      Ast_mapper_to0.default_mapper field
  in
  OUnit.assert_equal ~printer:(String.concat ", ") ["dead"; "as"]
    (List.map
       (fun (({txt} : string Asttypes.loc), _) -> txt)
       field0.pld_attributes)

let test_field_runtime_name_keeps_its_place_on_the_wire _ =
  let earlier = source_loc 10 20 and as_loc = source_loc 30 40 in
  let field =
    Ast_helper.Type.field ~loc
      ~attrs:
        [
          (located_string ~loc:earlier "dead", Parsetree.PStr []);
          ( located_string ~loc:as_loc "as",
            Parsetree.PStr
              [
                Ast_helper.Str.eval
                  (Ast_helper.Exp.constant
                     (Pconst_string (String_literal.string_from_semantic "wire")));
              ] );
        ]
      (located_string "a")
      (Ast_helper.Typ.constr ~loc (located_string (Longident.Lident "int")) [])
  in
  let field0 =
    Ast_mapper_to0.default_mapper.label_declaration
      Ast_mapper_to0.default_mapper field
  in
  OUnit.assert_equal ~printer:(String.concat ", ") ["dead"; "as"]
    (List.map
       (fun (({txt} : string Asttypes.loc), _) -> txt)
       field0.pld_attributes)

let test_constructor_runtime_tag_reaches_ast0_as_an_attribute _ =
  let as_attr =
    attr "as"
      (Parsetree.PStr
         [
           Ast_helper.Str.eval
             (Ast_helper.Exp.constant (Pconst_integer ("7", None)));
         ])
  in
  let constructor =
    Ast_helper.Type.constructor ~loc ~attrs:[as_attr] (located_string "Seven")
  in
  OUnit.assert_bool "Expected the @as attribute to become the runtime tag"
    (constructor.pcd_runtime_tag <> None);
  let constructor0 =
    Ast_mapper_to0.default_mapper.constructor_declaration
      Ast_mapper_to0.default_mapper constructor
  in
  OUnit.assert_bool "Expected a plain @as attribute on the ast0 wire"
    (has_attr "as" constructor0.pcd_attributes)

let map_expr0 e =
  Ast_mapper_from0.default_mapper.expr Ast_mapper_from0.default_mapper e

let map_value_binding0 vb =
  Ast_mapper_from0.default_mapper.value_binding Ast_mapper_from0.default_mapper
    vb

let to_value_binding0 vb =
  Ast_mapper_to0.default_mapper.value_binding Ast_mapper_to0.default_mapper vb

let test_value_constraint_roundtrips_through_ast0 _ =
  let newtype = Location.mknoloc "a" in
  let typ =
    Ast_helper.Typ.constr ~loc (Location.mknoloc (Longident.Lident "a")) []
  in
  let constraint_ = {Parsetree.pvc_newtypes = [newtype]; pvc_type = typ} in
  let vb =
    Ast_helper.Vb.mk ~loc ~constraint_
      (Ast_helper.Pat.var ~loc (Location.mknoloc "f"))
      (Ast_helper.Exp.ident ~loc (Location.mknoloc (Longident.Lident "x")))
  in
  let vb0 = to_value_binding0 vb in
  (match (vb0.pvb_pat.ppat_desc, vb0.pvb_expr.pexp_desc) with
  | ( Parsetree0.Ppat_constraint
        (_, {ptyp_desc = Ptyp_poly ([{txt = "a"}], {ptyp_desc = Ptyp_var "a"})}),
      Pexp_newtype
        ( {txt = "a"},
          {
            pexp_desc =
              Pexp_constraint
                (_, {ptyp_desc = Ptyp_constr ({txt = Lident "a"}, [])});
          } ) ) ->
    ()
  | _ ->
    assert_failure
      "Expected the locally abstract value constraint's v0 wrapper encoding");
  let mismatched_vb0 =
    match vb0.pvb_expr.pexp_desc with
    | Pexp_newtype (name, expr) ->
      {
        vb0 with
        pvb_expr =
          {
            vb0.pvb_expr with
            pexp_desc = Pexp_newtype ({name with txt = "b"}, expr);
          };
      }
    | _ -> assert_failure "Expected a leading legacy newtype"
  in
  let mismatched_vb = map_value_binding0 mismatched_vb0 in
  (match
     ( mismatched_vb.pvb_pat.ppat_desc,
       mismatched_vb.pvb_expr.pexp_desc,
       mismatched_vb.pvb_constraint )
   with
  | Ppat_constraint _, Pexp_extension extension, None ->
    let error = Builtin_attributes.error_of_extension extension in
    OUnit.assert_equal
      "A PPX returned a locally abstract type wrapper that does not enclose a \
       ReScript function. This v0 AST form is not supported."
      error.msg
  | _ -> assert_failure "A mismatched v0 wrapper structure must become an error");
  let vb = map_value_binding0 vb0 in
  match (vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc, vb.pvb_constraint) with
  | ( Ppat_var {txt = "f"},
      Pexp_ident {txt = Lident "x"},
      Some
        {
          pvc_newtypes = [{txt = "a"}];
          pvc_type = {ptyp_desc = Ptyp_constr ({txt = Lident "a"}, [])};
        } ) ->
    ()
  | _ ->
    assert_failure "Expected the structural value constraint after roundtrip"

(* A PPX can emit OCaml-style [function | p -> e]; the bridge must desugar it
   to [fun x -> match x with | p -> e] rather than crash. *)
let test_function_cases_desugar_to_fun_match _ =
  let case0 =
    {
      Parsetree0.pc_lhs = Ast_helper0.Pat.any ~loc ();
      pc_guard = None;
      pc_rhs =
        Ast_helper0.Exp.constant ~loc (Parsetree0.Pconst_integer ("1", None));
    }
  in
  let expr = map_expr0 (Ast_helper0.Exp.function_ ~loc [case0]) in
  match expr.pexp_desc with
  | Parsetree.Pexp_fun
      {
        params =
          [
            {
              p_lbl = Nolabel;
              p_default = None;
              p_pat = {ppat_desc = Ppat_var {txt = param}};
            };
          ];
        body =
          {
            pexp_desc =
              Pexp_match ({pexp_desc = Pexp_ident {txt = Lident scrutinee}}, [_]);
          };
      } ->
    OUnit.assert_equal ~msg:"scrutinee is the introduced parameter" param
      scrutinee
  | _ -> assert_failure "Expected fun x -> match x with ... after desugaring"

let map_expr_to0 e =
  Ast_mapper_to0.default_mapper.expr Ast_mapper_to0.default_mapper e

let map_pat_to0 p =
  Ast_mapper_to0.default_mapper.pat Ast_mapper_to0.default_mapper p

let attr_names attrs = List.map (fun ({Location.txt}, _) -> txt) attrs

let test_constructor_args_roundtrip_through_ast0 _ =
  let int_expr value =
    Ast_helper.Exp.constant ~loc (Parsetree.Pconst_integer (value, None))
  in
  let int_pat value =
    Ast_helper.Pat.constant ~loc (Parsetree.Pconst_integer (value, None))
  in
  let lid = Location.mknoloc (Longident.Lident "Pair") in
  let expr = Ast_helper.Exp.construct ~loc lid [int_expr "1"; int_expr "2"] in
  let expr0 = map_expr_to0 expr in
  (match expr0.pexp_desc with
  | Parsetree0.Pexp_construct (_, Some {pexp_desc = Pexp_tuple [_; _]}) ->
    OUnit.assert_bool "multiple arguments carry bridge metadata"
      (has_attr "_res.constructor_args" expr0.pexp_attributes)
  | _ -> assert_failure "Expected a tuple-encoded v0 constructor payload");
  let expr = map_expr0 expr0 in
  (match expr.pexp_desc with
  | Parsetree.Pexp_construct (_, {txt = [_; _]}) ->
    OUnit.assert_bool "bridge metadata is removed"
      (not (has_attr "_res.constructor_args" expr.pexp_attributes))
  | _ -> assert_failure "Expected two constructor arguments after roundtrip");
  let tuple_expr = Ast_helper.Exp.tuple ~loc [int_expr "1"; int_expr "2"] in
  let expr = Ast_helper.Exp.construct ~loc lid [tuple_expr] in
  let expr0 = map_expr_to0 expr in
  OUnit.assert_equal
    ~msg:"a single tuple argument does not carry bridge metadata" []
    expr0.pexp_attributes;
  let expr = map_expr0 expr0 in
  OUnit.assert_equal ~msg:"tuple roundtrip preserves empty attributes" []
    expr.pexp_attributes;
  (match expr.pexp_desc with
  | Parsetree.Pexp_construct (_, {txt = [{pexp_desc = Pexp_tuple [_; _]}]}) ->
    ()
  | _ -> assert_failure "Expected one tuple argument after roundtrip");
  let pat = Ast_helper.Pat.construct ~loc lid [int_pat "1"; int_pat "2"] in
  let pat0 = map_pat_to0 pat in
  (match pat0.ppat_desc with
  | Parsetree0.Ppat_construct (_, Some {ppat_desc = Ppat_tuple [_; _]}) ->
    OUnit.assert_bool "pattern arguments carry bridge metadata"
      (has_attr "_res.constructor_args" pat0.ppat_attributes)
  | _ -> assert_failure "Expected a tuple-encoded v0 constructor pattern");
  match (map_pat0 pat0).ppat_desc with
  | Parsetree.Ppat_construct (_, {txt = [_; _]}) -> ()
  | _ -> assert_failure "Expected two pattern arguments after roundtrip"

let check_args_keep_parentheses_location_in_ast0 sources =
  List.iter
    (fun source ->
      let parsed =
        Res_driver.parse_implementation_from_source
          ~display_filename:"VariantArgsLocation.res" ~source
      in
      let pat, expr =
        match parsed.parsetree with
        | [{pstr_desc = Pstr_value (_, [{pvb_pat; pvb_expr}])}] ->
          (pvb_pat, pvb_expr)
        | _ -> assert_failure "Expected one constructor binding"
      in
      let pattern_start = String.index source '(' in
      let pattern_end = 1 + String.index_from source pattern_start ')' in
      let expression_start = String.index_from source pattern_end '(' in
      let expression_end = 1 + String.index_from source expression_start ')' in
      OUnit.assert_equal [] pat.ppat_attributes;
      OUnit.assert_equal [] expr.pexp_attributes;
      let check ?(offset = 0) (pat : Parsetree0.pattern)
          (expr : Parsetree0.expression) =
        let assert_loc start finish {Location.loc_start; loc_end} =
          OUnit.assert_equal (start + offset) loc_start.pos_cnum;
          OUnit.assert_equal (finish + offset) loc_end.pos_cnum
        in
        (match pat.ppat_desc with
        | Ppat_construct (_, Some {ppat_desc = Ppat_tuple _; ppat_loc})
        | Ppat_variant (_, Some {ppat_desc = Ppat_tuple _; ppat_loc}) ->
          assert_loc pattern_start pattern_end ppat_loc
        | _ -> assert_failure "Expected v0 constructor pattern tuple");
        match expr.pexp_desc with
        | Pexp_construct (_, Some {pexp_desc = Pexp_tuple _; pexp_loc})
        | Pexp_variant (_, Some {pexp_desc = Pexp_tuple _; pexp_loc}) ->
          assert_loc expression_start expression_end pexp_loc
        | _ -> assert_failure "Expected v0 constructor expression tuple"
      in
      let pat0 = map_pat_to0 pat in
      let expr0 = map_expr_to0 expr in
      check pat0 expr0;
      check (map_pat_to0 (map_pat0 pat0)) (map_expr_to0 (map_expr0 expr0));
      let shift_loc _ (loc : Location.t) =
        {
          loc with
          loc_start =
            {loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + 100};
          loc_end = {loc.loc_end with pos_cnum = loc.loc_end.pos_cnum + 100};
        }
      in
      let to0 = {Ast_mapper_to0.default_mapper with location = shift_loc} in
      check ~offset:100 (to0.pat to0 pat) (to0.expr to0 expr);
      let from0 = {Ast_mapper_from0.default_mapper with location = shift_loc} in
      check ~offset:100
        (map_pat_to0 (from0.pat from0 pat0))
        (map_expr_to0 (from0.expr from0 expr0)))
    sources

let test_constructor_args_keep_parentheses_location_in_ast0 _ =
  check_args_keep_parentheses_location_in_ast0
    [
      "let Pair(a, b) = Pair(1, 2)";
      "let Pair  (a, b) = Pair  (1, 2)";
      "let Pair /* pattern */ (a, b) = Pair /* expression */ (1, 2)";
      "let Module.Pair(a,\n b) = Module.Pair(1,\n 2)";
    ]

let test_polyvariant_args_keep_parentheses_location_in_ast0 _ =
  check_args_keep_parentheses_location_in_ast0
    [
      "let #Pair(a, b) = #Pair(1, 2)";
      "let #Pair /* pattern */ (a, b) = #Pair /* expression */ (1, 2)";
      "let #\"quoted label\"(a,\n b) = #\"quoted label\"(1,\n 2)";
    ]

let test_constructor_argument_locations _ =
  let pattern_args_loc (pat : Parsetree.pattern) =
    match pat.ppat_desc with
    | Ppat_construct (_, {loc}) | Ppat_variant (_, {loc}) -> loc
    | _ -> assert_failure "Expected a constructor pattern"
  in
  let expression_args_loc (expr : Parsetree.expression) =
    match expr.pexp_desc with
    | Pexp_construct (_, {loc}) | Pexp_variant (_, {loc}) -> loc
    | _ -> assert_failure "Expected a constructor expression"
  in
  let shift_loc _ (loc : Location.t) =
    {
      loc with
      loc_start = {loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + 100};
      loc_end = {loc.loc_end with pos_cnum = loc.loc_end.pos_cnum + 100};
    }
  in
  List.iter
    (fun source ->
      let parsed =
        Res_driver.parse_implementation_from_source
          ~display_filename:"ArgumentLocations.res" ~source
      in
      OUnit.assert_bool "source parses" (not parsed.invalid);
      let pat, expr =
        match parsed.parsetree with
        | [{pstr_desc = Pstr_value (_, [{pvb_pat; pvb_expr}])}] ->
          (pvb_pat, pvb_expr)
        | _ -> assert_failure "Expected a constructor binding"
      in
      let pat_loc = pattern_args_loc pat in
      let expr_loc = expression_args_loc expr in
      let expected_pat_bridge_loc =
        match pat.ppat_desc with
        | Ppat_construct (_, {txt = [arg]}) | Ppat_variant (_, {txt = [arg]}) ->
          arg.ppat_loc
        | _ -> pat_loc
      in
      let expected_expr_bridge_loc =
        match expr.pexp_desc with
        | Pexp_construct (_, {txt = [arg]}) | Pexp_variant (_, {txt = [arg]}) ->
          arg.pexp_loc
        | _ -> expr_loc
      in
      OUnit.assert_equal ~msg:"v0 uses the payload span for a single argument"
        expected_pat_bridge_loc
        (pattern_args_loc (map_pat0 (map_pat_to0 pat)));
      OUnit.assert_equal ~msg:"v0 uses the payload span for a single argument"
        expected_expr_bridge_loc
        (expression_args_loc (map_expr0 (map_expr_to0 expr)));
      let equals = String.index source '=' in
      let assert_span start finish (loc : Location.t) =
        OUnit.assert_equal start loc.loc_start.pos_cnum;
        OUnit.assert_equal finish loc.loc_end.pos_cnum
      in
      if String.contains source '(' then (
        assert_span (String.index source '(')
          (1 + String.rindex_from source equals ')')
          pat_loc;
        assert_span
          (String.index_from source equals '(')
          (1 + String.rindex source ')')
          expr_loc)
      else (
        OUnit.assert_equal pat.ppat_loc pat_loc;
        OUnit.assert_equal expr.pexp_loc expr_loc);
      let mapper = Ast_mapper.default_mapper in
      OUnit.assert_equal pat (mapper.pat mapper pat);
      OUnit.assert_equal expr (mapper.expr mapper expr);
      let mapper = {mapper with location = shift_loc} in
      OUnit.assert_equal (shift_loc () pat_loc)
        (pattern_args_loc (mapper.pat mapper pat));
      OUnit.assert_equal (shift_loc () expr_loc)
        (expression_args_loc (mapper.expr mapper expr));
      let visited = ref [] in
      let iterator =
        {
          Ast_iterator.default_iterator with
          location = (fun _ loc -> visited := loc :: !visited);
        }
      in
      iterator.pat iterator pat;
      OUnit.assert_bool "iterator visits pattern argument span"
        (List.mem pat_loc !visited);
      visited := [];
      iterator.expr iterator expr;
      OUnit.assert_bool "iterator visits expression argument span"
        (List.mem expr_loc !visited))
    [
      "let Pair /* pattern */ (a, b) = Pair /* expression */ (1, 2)";
      "let Pair((a, b)) = Pair((1, 2))";
      "let Single(a) = Single(1)";
      "let Unit() = Unit()";
      "let Empty = Empty";
      "let #Pair /* pattern */ (a, b) = #Pair /* expression */ (1, 2)";
      "let #Pair((a, b)) = #Pair((1, 2))";
      "let #Single(a) = #Single(1)";
      "let #Unit() = #Unit()";
      "let #Empty = #Empty";
    ]

let test_incomplete_constructor_argument_locations _ =
  List.iter
    (fun source ->
      let parsed =
        Res_driver.parse_implementation_from_source
          ~display_filename:"IncompleteConstructor.res" ~source
      in
      let args_loc =
        match parsed.parsetree with
        | [
         {
           pstr_desc =
             Pstr_value
               (_, [{pvb_expr = {pexp_desc = Pexp_construct (_, {loc})}}]);
         };
        ] ->
          loc
        | [
         {
           pstr_desc =
             Pstr_value
               ( _,
                 [
                   {
                     pvb_expr =
                       {
                         pexp_desc =
                           Pexp_fun
                             {
                               body =
                                 {
                                   pexp_desc =
                                     Pexp_match
                                       ( _,
                                         [
                                           {
                                             pc_lhs =
                                               {
                                                 ppat_desc =
                                                   Ppat_construct (_, {loc});
                                               };
                                           };
                                         ] );
                                 };
                             };
                       };
                   };
                 ] );
         };
        ] ->
          loc
        | _ -> assert_failure "Expected an incomplete constructor argument list"
      in
      let cursor = String.length source - 1 in
      OUnit.assert_equal (String.index source '(') args_loc.loc_start.pos_cnum;
      OUnit.assert_bool "recovery span includes the character before the cursor"
        (args_loc.loc_start.pos_cnum <= cursor
        && cursor < args_loc.loc_end.pos_cnum))
    [
      "let value = Pair(";
      "let value = Pair(1,";
      "let read = value => switch value { | Pair(";
      "let read = value => switch value { | Pair(a,";
    ]

let test_constructor_normalization_keeps_argument_locations _ =
  List.iter
    (fun source ->
      let parsed =
        Res_driver.parse_implementation_from_source
          ~display_filename:"NormalizedArgumentLocations.res" ~source
      in
      OUnit.assert_bool "source parses" (not parsed.invalid);
      let pat, expr =
        match (Ext_list.last parsed.parsetree).pstr_desc with
        | Pstr_value (_, [{pvb_pat; pvb_expr}]) -> (pvb_pat, pvb_expr)
        | _ -> assert_failure "Expected a constructor binding"
      in
      let expected_pat_loc =
        match pat.ppat_desc with
        | Ppat_construct (_, {loc}) | Ppat_variant (_, {loc}) -> loc
        | _ -> assert_failure "Expected a constructor pattern"
      in
      let expected_expr_loc =
        match expr.pexp_desc with
        | Pexp_construct (_, {loc}) | Pexp_variant (_, {loc}) -> loc
        | _ -> assert_failure "Expected a constructor expression"
      in
      let typed, _, _ =
        Typemod.type_structure Env.initial_safe_string parsed.parsetree loc
      in
      let pat, expr =
        match (Ext_list.last typed.str_items).str_desc with
        | Tstr_value (_, [{vb_pat; vb_expr}]) -> (vb_pat, vb_expr)
        | _ -> assert_failure "Expected a typed constructor binding"
      in
      (match pat.pat_desc with
      | Tpat_construct (_, _, [{pat_desc = Tpat_tuple [_; _]; pat_loc}])
      | Tpat_variant (_, Some {pat_desc = Tpat_tuple [_; _]; pat_loc}, _) ->
        OUnit.assert_equal expected_pat_loc pat_loc
      | _ -> assert_failure "Expected a typed tuple payload pattern");
      match expr.exp_desc with
      | Texp_construct (_, _, [{exp_desc = Texp_tuple [_; _]; exp_loc}])
      | Texp_variant (_, Some {exp_desc = Texp_tuple [_; _]; exp_loc}) ->
        OUnit.assert_equal expected_expr_loc exp_loc
      | _ -> assert_failure "Expected a typed tuple payload expression")
    [
      "type t = Pair((int, int))\n\
       let Pair /* pattern */ (a, b) = Pair /* expression */ (1, 2)";
      "let #Pair /* pattern */ (a, b) = #Pair /* expression */ (1, 2)";
    ]

let test_fresh_ast0_constructor_tuple_reprints_without_internal_metadata _ =
  let int_expr value =
    Ast_helper0.Exp.constant ~loc (Parsetree0.Pconst_integer (value, None))
  in
  let expr =
    map_expr0
      (Ast_helper0.Exp.construct ~loc
         (Location.mknoloc (Longident.Lident "Pair"))
         (Some (Ast_helper0.Exp.tuple ~loc [int_expr "1"; int_expr "2"])))
  in
  let pat =
    map_pat0
      (Ast_helper0.Pat.construct ~loc
         (Location.mknoloc (Longident.Lident "Pair"))
         (Some
            (Ast_helper0.Pat.tuple ~loc
               [
                 Ast_helper0.Pat.var ~loc (Location.mknoloc "a");
                 Ast_helper0.Pat.var ~loc (Location.mknoloc "b");
               ])))
  in
  OUnit.assert_equal ~msg:"fresh v0 expression needs no internal metadata" []
    expr.pexp_attributes;
  OUnit.assert_equal ~msg:"fresh v0 pattern needs no internal metadata" []
    pat.ppat_attributes;
  List.iter
    (fun width ->
      let printed =
        Res_printer.print_implementation
          [
            Ast_helper.Str.value ~loc Nonrecursive
              [Ast_helper.Vb.mk ~loc pat expr];
          ]
          ~comments:[] ~width
      in
      let parsed =
        Res_driver.parse_implementation_from_source
          ~display_filename:"FreshAst0Constructor.res" ~source:printed
      in
      match parsed.parsetree with
      | [
       {
         pstr_desc =
           Pstr_value
             ( _,
               [
                 {
                   pvb_pat =
                     {
                       ppat_desc =
                         Ppat_construct
                           (_, {txt = [{ppat_desc = Ppat_tuple [_; _]}]});
                     };
                   pvb_expr =
                     {
                       pexp_desc =
                         Pexp_construct
                           (_, {txt = [{pexp_desc = Pexp_tuple [_; _]}]});
                     };
                 };
               ] );
       };
      ] ->
        ()
      | _ -> assert_failure "Expected a printed tuple payload")
    [10; 80]

let test_ast0_explicit_arity_becomes_constructor_args _ =
  let arg value =
    Ast_helper0.Exp.constant ~loc (Parsetree0.Pconst_integer (value, None))
  in
  let expr0 =
    Ast_helper0.Exp.construct ~loc
      ~attrs:[attr "ocaml.explicit_arity" (Parsetree0.PStr [])]
      (Location.mknoloc (Longident.Lident "Pair"))
      (Some (Ast_helper0.Exp.tuple ~loc [arg "1"; arg "2"]))
  in
  match (map_expr0 expr0).pexp_desc with
  | Parsetree.Pexp_construct (_, {txt = [_; _]}) -> ()
  | _ -> assert_failure "Expected explicit-arity v0 payload to become arguments"

let test_fresh_ast0_constructor_tuple_defers_arity_to_typechecker _ =
  let int_expr value =
    Ast_helper0.Exp.constant ~loc (Parsetree0.Pconst_integer (value, None))
  in
  let int_pat value =
    Ast_helper0.Pat.constant ~loc (Parsetree0.Pconst_integer (value, None))
  in
  let lid = Location.mknoloc (Longident.Lident "FreshPairForAst0") in
  let expr =
    map_expr0
      (Ast_helper0.Exp.construct ~loc lid
         (Some (Ast_helper0.Exp.tuple ~loc [int_expr "1"; int_expr "2"])))
  in
  let pat =
    map_pat0
      (Ast_helper0.Pat.construct ~loc lid
         (Some (Ast_helper0.Pat.tuple ~loc [int_pat "1"; int_pat "2"])))
  in
  OUnit.assert_equal [] expr.pexp_attributes;
  OUnit.assert_equal [] pat.ppat_attributes;
  List.iter
    (fun payload_type ->
      let parsed =
        Res_driver.parse_implementation_from_source
          ~display_filename:"Ast0ConstructorArgsTest.res"
          ~source:
            (Printf.sprintf
               "type freshPairForAst0 = FreshPairForAst0(%s)\n\
                let value = FreshPairForAst0(1, 2)\n\
                let FreshPairForAst0(a, b) = value"
               payload_type)
      in
      let structure =
        match parsed.parsetree with
        | [type_item; value_item; pattern_item] ->
          let value_item =
            match value_item.pstr_desc with
            | Pstr_value (rec_flag, [binding]) ->
              {
                value_item with
                pstr_desc =
                  Pstr_value (rec_flag, [{binding with pvb_expr = expr}]);
              }
            | _ -> assert_failure "Expected value binding"
          in
          let pattern_item =
            match pattern_item.pstr_desc with
            | Pstr_value (rec_flag, [binding]) ->
              {
                pattern_item with
                pstr_desc = Pstr_value (rec_flag, [{binding with pvb_pat = pat}]);
              }
            | _ -> assert_failure "Expected pattern binding"
          in
          [type_item; value_item; pattern_item]
        | _ -> assert_failure "Expected type declaration and two value bindings"
      in
      ignore (Typemod.type_structure Env.initial_safe_string structure loc))
    ["int, int"; "(int, int)"]

let test_polyvariant_args_roundtrip_through_ast0 _ =
  let int_expr value =
    Ast_helper.Exp.constant ~loc (Parsetree.Pconst_integer (value, None))
  in
  let int_pat value =
    Ast_helper.Pat.constant ~loc (Parsetree.Pconst_integer (value, None))
  in
  let expr = Ast_helper.Exp.variant ~loc "Pair" [int_expr "1"; int_expr "2"] in
  let expr0 = map_expr_to0 expr in
  (match expr0.pexp_desc with
  | Parsetree0.Pexp_variant ("Pair", Some {pexp_desc = Pexp_tuple [_; _]}) ->
    OUnit.assert_bool "polymorphic variant arguments carry bridge metadata"
      (has_attr "_res.constructor_args" expr0.pexp_attributes)
  | _ -> assert_failure "Expected tuple-encoded v0 polymorphic variant payload");
  (match (map_expr0 expr0).pexp_desc with
  | Parsetree.Pexp_variant ("Pair", {txt = [_; _]}) -> ()
  | _ -> assert_failure "Expected two polymorphic variant arguments");
  let pat = Ast_helper.Pat.variant ~loc "Pair" [int_pat "1"; int_pat "2"] in
  let pat0 = map_pat_to0 pat in
  (match pat0.ppat_desc with
  | Parsetree0.Ppat_variant ("Pair", Some {ppat_desc = Ppat_tuple [_; _]}) ->
    OUnit.assert_bool "polymorphic variant pattern arguments carry metadata"
      (has_attr "_res.constructor_args" pat0.ppat_attributes)
  | _ -> assert_failure "Expected tuple-encoded v0 polymorphic variant pattern");
  (match (map_pat0 pat0).ppat_desc with
  | Parsetree.Ppat_variant ("Pair", {txt = [_; _]}) -> ()
  | _ -> assert_failure "Expected two polymorphic variant pattern arguments");
  let int_type =
    Ast_helper.Typ.constr ~loc (Location.mknoloc (Longident.Lident "int")) []
  in
  let typ =
    Ast_helper.Typ.variant ~loc
      [
        Parsetree.Rtag
          ( Location.mknoloc "Pair",
            [],
            false,
            [Location.mkloc [int_type; int_type] loc] );
      ]
      Closed None
  in
  let typ0 =
    Ast_mapper_to0.default_mapper.typ Ast_mapper_to0.default_mapper typ
  in
  (match typ0.ptyp_desc with
  | Parsetree0.Ptyp_variant
      ( [Rtag ({txt = "Pair"}, _, false, [{ptyp_desc = Ptyp_tuple [_; _]}])],
        _,
        _ ) ->
    ()
  | _ -> assert_failure "Expected tuple-encoded v0 polymorphic variant type");
  let typ =
    Ast_mapper_from0.default_mapper.typ Ast_mapper_from0.default_mapper typ0
  in
  match typ.ptyp_desc with
  | Parsetree.Ptyp_variant
      ([Rtag ({txt = "Pair"}, _, false, [{txt = [_; _]}])], _, _) ->
    ()
  | _ -> assert_failure "Expected two polymorphic variant type arguments"

let assert_string_expr ~expected_source ~expected_semantic expr =
  match expr.Parsetree.pexp_desc with
  | Pexp_constant (Pconst_string payload) ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected_source
      (String_literal.string_source payload);
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected_semantic
      (String_literal.string_semantic payload)
  | _ -> assert_failure "Expected a string expression"

let assert_string_pat ~expected_source ~expected_semantic pat =
  match pat.Parsetree.ppat_desc with
  | Ppat_constant (Pconst_string payload) ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected_source
      (String_literal.string_source payload);
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected_semantic
      (String_literal.string_semantic payload)
  | _ -> assert_failure "Expected a string pattern"

let assert_template_expr ~expected expr =
  match expr.Parsetree.pexp_desc with
  | Pexp_template {source_segments = [actual]; values = []} ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected actual.txt
  | _ -> assert_failure "Expected a template expression"

let test_ast0_strings_convert_to_internal_representation _ =
  let encoded = {|a\n\uD83D\uDE00|} in
  let expr0 =
    Ast_helper0.Exp.constant ~loc
      (Parsetree0.Pconst_string (encoded, Some "js"))
  in
  assert_string_expr ~expected_source:encoded ~expected_semantic:"a\n😀"
    (map_expr0 expr0);
  let pat0 =
    Ast_helper0.Pat.constant ~loc
      (Parsetree0.Pconst_string (encoded, Some "js"))
  in
  assert_string_pat ~expected_source:encoded ~expected_semantic:"a\n😀"
    (map_pat0 pat0);
  (* Older compiler-produced ast0 files can contain the processed [*j]
     delimiter. Decode those directly instead of interpreting them as source
     text again. *)
  let legacy_expr0 =
    Ast_helper0.Exp.constant ~loc (Parsetree0.Pconst_string ({|\"|}, Some "*j"))
  in
  assert_string_expr ~expected_source:{|\"|} ~expected_semantic:"\""
    (map_expr0 legacy_expr0);
  let template_expr0 =
    Ast_helper0.Exp.constant ~loc
      ~attrs:[attr "res.template" (Parsetree0.PStr [])]
      (Parsetree0.Pconst_string (encoded, Some "js"))
  in
  assert_template_expr ~expected:encoded (map_expr0 template_expr0);
  let quoted_expr0 =
    Ast_helper0.Exp.constant ~loc
      (Parsetree0.Pconst_string ({|\x61|}, Some "custom"))
  in
  assert_string_expr ~expected_source:{|\\x61|} ~expected_semantic:{|\x61|}
    (map_expr0 quoted_expr0);
  (* A tagged pattern cannot invoke its tag. Treating its raw contents as a
     string made json`\x61` collide with the ordinary "\\x61" pattern during
     string-switch sorting. Reject both known and arbitrary PPX delimiters. *)
  List.iter
    (fun tag ->
      let tagged_pattern0 =
        Ast_helper0.Pat.constant ~loc
          (Parsetree0.Pconst_string ({|\x61|}, Some tag))
      in
      match map_pat0 tagged_pattern0 with
      | _ -> assert_failure "Expected the ast0 tagged pattern to be rejected"
      | exception Location.Error _ -> ())
    ["custom"; "json"];
  let invalid_expr0 =
    Ast_helper0.Exp.constant ~loc
      (Parsetree0.Pconst_string ({|\uD800|}, Some "js"))
  in
  match map_expr0 invalid_expr0 with
  | _ -> assert_failure "Expected an invalid ast0 string escape"
  | exception Location.Error _ -> ()

let test_ppx_byte_strings_convert_to_valid_utf8 _ =
  let byte_string = "a\xff\xc3\xa9" in
  let expression0 =
    Ast_helper0.Exp.constant ~loc (Ast_helper0.Const.string byte_string)
  in
  match (map_expr0 expression0).pexp_desc with
  | Pexp_constant (Pconst_string payload) ->
    let source = String_literal.string_source payload in
    let semantic = String_literal.string_semantic payload in
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") "aÿé" source;
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") "aÿé" semantic;
    OUnit.assert_equal ~printer:string_of_int 3
      (String_literal.utf16_length semantic)
  | _ -> assert_failure "Expected a normalized PPX string"

let test_string_literals_roundtrip_through_ast0 _ =
  let semantic = "a\n😀" in
  let expr = Ast_helper.Exp.constant ~loc (Ast_helper.Const.string semantic) in
  assert_string_expr ~expected_source:{|a\n😀|} ~expected_semantic:semantic
    (map_expr0 (map_expr_to0 expr));
  let encoded = {|a\n\uD83D\uDE00|} in
  let template_expr =
    Ast_helper.Exp.template ~loc [located_string encoded] []
  in
  let template_expr0 = map_expr_to0 template_expr in
  (match template_expr0.Parsetree0.pexp_desc with
  | Pexp_constant (Pconst_string (actual, Some "js")) ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") encoded actual;
    OUnit.assert_bool "expected the ast0 template marker"
      (List.mem "res.template" (attr_names template_expr0.pexp_attributes))
  | _ -> assert_failure "Expected ast0's template string representation");
  let template_expr = map_expr0 template_expr0 in
  assert_template_expr ~expected:encoded template_expr;
  OUnit.assert_bool "the ast0 template marker was consumed"
    (not (List.mem "res.template" (attr_names template_expr.pexp_attributes)));
  let template_pat =
    Ast_helper.Pat.constant ~loc (Ast_helper.Const.string "a\n😀")
  in
  let template_pat0 =
    Ast_mapper_to0.default_mapper.pat Ast_mapper_to0.default_mapper template_pat
  in
  OUnit.assert_bool "ordinary string patterns need no ast0 template marker"
    (not (List.mem "res.template" (attr_names template_pat0.ppat_attributes)));
  let template_pat = map_pat0 template_pat0 in
  (match template_pat.ppat_desc with
  | Ppat_constant (Pconst_string payload) ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") "a\n😀"
      (String_literal.string_semantic payload)
  | _ -> assert_failure "Expected a string pattern after ast0 roundtrip");
  let json_expr =
    Ast_helper.Exp.constant ~loc (Parsetree.Pconst_json {|{"answer":42}|})
  in
  (match (map_expr0 (map_expr_to0 json_expr)).pexp_desc with
  | Pexp_constant (Pconst_json actual) ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") {|{"answer":42}|} actual
  | _ -> assert_failure "Expected a JSON literal");
  let char_pattern =
    Ast_helper.Pat.constant ~loc
      (Parsetree.Pconst_char {source = {|\u{61}|}; semantic = 0x61})
  in
  let char_pattern0 =
    Ast_mapper_to0.default_mapper.pat Ast_mapper_to0.default_mapper char_pattern
  in
  (match char_pattern0.ppat_desc with
  | Ppat_constant (Pconst_char actual) ->
    OUnit.assert_equal ~printer:string_of_int 0x61 actual
  | _ -> assert_failure "Expected an ast0 character literal");
  (match (map_pat0 char_pattern0).ppat_desc with
  | Ppat_constant (Pconst_char {source; semantic}) ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") "a" source;
    OUnit.assert_equal ~printer:string_of_int 0x61 semantic
  | _ -> assert_failure "Expected a character literal after ast0 roundtrip");
  let source_string =
    Ast_helper.Exp.constant ~loc
      (Parsetree.Pconst_string
         (match String_literal.string_from_source {|\x61|} with
         | Some payload -> payload
         | None -> assert_failure "Expected a valid source string"))
  in
  assert_string_expr ~expected_source:{|\x61|} ~expected_semantic:"a"
    (map_expr0 (map_expr_to0 source_string))

let assert_raw_extension_payload ~name ~expected expression =
  match expression.Parsetree.pexp_desc with
  | Pexp_extension
      ( {txt},
        PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ({pexp_desc = Pexp_constant (Pconst_raw_source actual)}, _);
            };
          ] ) ->
    OUnit.assert_equal name txt;
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected actual
  | _ -> assert_failure "Expected a raw extension string payload"

let test_raw_extension_payloads_roundtrip_through_ast0 _ =
  let encoded = {|'\\n'|} in
  List.iter
    (fun name ->
      let payload =
        Parsetree0.PStr
          [
            Ast_helper0.Str.eval ~loc
              (Ast_helper0.Exp.constant ~loc
                 (Parsetree0.Pconst_string (encoded, Some "js")));
          ]
      in
      let expression0 =
        Ast_helper0.Exp.extension ~loc (Location.mknoloc name, payload)
      in
      let expression = map_expr0 expression0 in
      assert_raw_extension_payload ~name ~expected:encoded expression;
      assert_raw_extension_payload ~name ~expected:encoded
        (map_expr0 (map_expr_to0 expression)))
    ["raw"; "ffi"; "re"]

let test_tagged_templates_roundtrip_through_ast0 _ =
  let head_loc = source_loc 4 16 in
  let tail_loc = source_loc 20 25 in
  let tag =
    Ast_helper.Exp.ident ~loc (Location.mknoloc (Longident.Lident "tag"))
  in
  let value = Ast_helper.Exp.constant ~loc (Ast_helper.Const.integer "1") in
  let expression =
    Ast_helper.Exp.tagged_template ~loc
      ~attrs:[attr "keep" (Parsetree.PStr [])]
      tag
      [
        located_string ~loc:head_loc {|raw\unicode|};
        located_string ~loc:tail_loc " tail";
      ]
      [value]
  in
  let expression0 = map_expr_to0 expression in
  OUnit.assert_bool "the frozen AST uses the tagged-template marker"
    (List.mem "res.taggedTemplate" (attr_names expression0.pexp_attributes));
  (match expression0.pexp_desc with
  | Pexp_apply
      ( _,
        [
          (_, {pexp_desc = Pexp_array [head; tail]});
          (_, {pexp_desc = Pexp_array [_]});
        ] ) ->
    OUnit.assert_equal ~printer:Ext_obj.dump [head_loc; tail_loc]
      [head.pexp_loc; tail.pexp_loc]
  | _ -> assert_failure "Expected frozen-AST tagged-template arrays");
  match (map_expr0 expression0).pexp_desc with
  | Pexp_tagged_template
      {
        tag = {pexp_desc = Pexp_ident {txt = Longident.Lident "tag"}};
        raw_sources;
        values = [{pexp_desc = Pexp_constant (Pconst_integer ("1", None))}];
      } ->
    OUnit.assert_equal ~printer:Ext_obj.dump [{|raw\unicode|}; " tail"]
      (List.map (fun {Location.txt} -> txt) raw_sources);
    OUnit.assert_equal ~printer:Ext_obj.dump [head_loc; tail_loc]
      (List.map (fun (source : string Location.loc) -> source.loc) raw_sources);
    OUnit.assert_equal ["keep"]
      (attr_names (map_expr0 expression0).pexp_attributes)
  | _ -> assert_failure "Expected an explicit tagged template after roundtrip"

let test_ppx_rewritten_tagged_template_segments _ =
  let semantic = "${value}`\\" in
  let segment =
    Ast_helper0.Exp.constant ~loc (Ast_helper0.Const.string semantic)
  in
  let tag =
    Ast_helper0.Exp.ident ~loc (Location.mknoloc (Longident.Lident "tag"))
  in
  let expression0 =
    Ast_helper0.Exp.apply ~loc
      ~attrs:[attr "res.taggedTemplate" (Parsetree0.PStr [])]
      tag
      [
        (Nolabel, Ast_helper0.Exp.array ~loc [segment]);
        (Nolabel, Ast_helper0.Exp.array ~loc []);
      ]
  in
  match (map_expr0 expression0).pexp_desc with
  | Pexp_tagged_template {raw_sources = [{txt}]} ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") {e|\${value}\`\\|e} txt
  | _ -> assert_failure "Expected a rewritten tagged template after roundtrip"

let test_ppx_rewritten_template_segments _ =
  let template_attr = attr "res.template" (Parsetree0.PStr []) in
  let semantic = "${value}`\\" in
  let uninterpolated =
    Ast_helper0.Exp.constant ~loc ~attrs:[template_attr]
      (Ast_helper0.Const.string semantic)
  in
  assert_template_expr ~expected:{e|\${value}\`\\|e} (map_expr0 uninterpolated);
  let segment semantic =
    Ast_helper0.Exp.constant ~loc ~attrs:[template_attr]
      (Ast_helper0.Const.string semantic)
  in
  let concat lhs rhs =
    Ast_helper0.Exp.apply ~loc ~attrs:[template_attr]
      (Ast_helper0.Exp.ident ~loc (Location.mknoloc (Longident.Lident "^")))
      [(Asttypes.Noloc.Nolabel, lhs); (Asttypes.Noloc.Nolabel, rhs)]
  in
  let value =
    Ast_helper0.Exp.ident ~loc (Location.mknoloc (Longident.Lident "value"))
  in
  let interpolated =
    concat (concat (segment "${head}") value) (segment "`\\")
  in
  match (map_expr0 interpolated).pexp_desc with
  | Pexp_template
      {
        source_segments = [{txt = head}; {txt = tail}];
        values = [{pexp_desc = Pexp_ident {txt = Longident.Lident "value"}}];
      } ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") {e|\${head}|e} head;
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") {e|\`\\|e} tail
  | _ -> assert_failure "Expected a rewritten template after roundtrip"

let test_interpolated_templates_roundtrip_through_ast0 _ =
  let head_loc = source_loc 1 8 in
  let tail_loc = source_loc 12 16 in
  let value = Ast_helper.Exp.constant ~loc (Ast_helper.Const.integer "1") in
  let expression =
    Ast_helper.Exp.template ~loc
      ~attrs:[attr "keep" (Parsetree.PStr [])]
      [
        located_string ~loc:head_loc {|head\n|};
        located_string ~loc:tail_loc "tail";
      ]
      [value]
  in
  let expression0 = map_expr_to0 expression in
  OUnit.assert_bool "the frozen AST uses the template marker"
    (List.mem "res.template" (attr_names expression0.pexp_attributes));
  let rec first_segment (expression : Parsetree0.expression) =
    match expression.pexp_desc with
    | Pexp_apply (_, [(_, lhs); (_, _)]) -> first_segment lhs
    | Pexp_constant (Pconst_string (source, Some actual_delimiter)) ->
      OUnit.assert_equal "js" actual_delimiter;
      OUnit.assert_equal {|head\n|} source
    | _ -> assert_failure "Expected a frozen-AST template segment"
  in
  first_segment expression0;
  let rec segment_locations (expression : Parsetree0.expression) =
    match expression.pexp_desc with
    | Pexp_apply (_, [(_, lhs); (_, rhs)]) ->
      segment_locations lhs @ segment_locations rhs
    | Pexp_constant (Pconst_string (_, Some "js")) -> [expression.pexp_loc]
    | _ -> []
  in
  OUnit.assert_equal ~printer:Ext_obj.dump [head_loc; tail_loc]
    (segment_locations expression0);
  match map_expr0 expression0 with
  | {
   pexp_desc =
     Pexp_template
       {
         source_segments;
         values = [{pexp_desc = Pexp_constant (Pconst_integer ("1", None))}];
       };
   pexp_attributes;
  } ->
    OUnit.assert_equal ~printer:Ext_obj.dump [{|head\n|}; "tail"]
      (List.map (fun {Location.txt} -> txt) source_segments);
    OUnit.assert_equal ~printer:Ext_obj.dump [head_loc; tail_loc]
      (List.map
         (fun (source : string Location.loc) -> source.loc)
         source_segments);
    OUnit.assert_equal ["keep"] (attr_names pexp_attributes)
  | _ -> assert_failure "Expected an explicit template after roundtrip"

let test_ast0_json_interpolation_is_rejected _ =
  let template_attr = attr "res.template" (Parsetree0.PStr []) in
  let segment source =
    Ast_helper0.Exp.constant ~loc ~attrs:[template_attr]
      (Parsetree0.Pconst_string (source, Some "json"))
  in
  let concat lhs rhs =
    Ast_helper0.Exp.apply ~loc ~attrs:[template_attr]
      (Ast_helper0.Exp.ident ~loc (Location.mknoloc (Longident.Lident "^")))
      [(Asttypes.Noloc.Nolabel, lhs); (Asttypes.Noloc.Nolabel, rhs)]
  in
  let value =
    Ast_helper0.Exp.constant ~loc (Parsetree0.Pconst_integer ("1", None))
  in
  let expression = concat (concat (segment "head") value) (segment "tail") in
  match map_expr0 expression with
  | _ -> assert_failure "Expected ast0 JSON interpolation to be rejected"
  | exception Location.Error _ -> ()

let test_string_source_reprints_after_ast0_roundtrip _ =
  let source =
    {|let newline = "\n"
let slashN = "\\n"
let quote = "\""
let slash = "\\"|}
  in
  let parsed =
    Res_driver.parse_implementation_from_source
      ~display_filename:"StringReprintTest.res" ~source
  in
  OUnit.assert_bool "expected valid ReScript source" (not parsed.invalid);
  let structure0 =
    Ast_mapper_to0.default_mapper.structure Ast_mapper_to0.default_mapper
      parsed.parsetree
  in
  let round_tripped =
    Ast_mapper_from0.default_mapper.structure Ast_mapper_from0.default_mapper
      structure0
  in
  let reprinted =
    Res_printer.print_implementation round_tripped ~comments:[] ~width:80
  in
  OUnit.assert_equal ~printer:(Printf.sprintf "%S") (source ^ "\n") reprinted

let test_invalid_utf8_doc_comment_roundtrips_through_ast0 _ =
  let source = "/** doc " ^ "\xff" ^ " byte */\nlet value = 1" in
  let parsed =
    Res_driver.parse_implementation_from_source
      ~display_filename:"InvalidDocComment.res" ~source
  in
  OUnit.assert_bool "expected invalid UTF-8 to be diagnosed" parsed.invalid;
  OUnit.assert_bool "expected an invalid-code-point diagnostic"
    (List.exists
       (fun diagnostic ->
         Res_diagnostics.explain diagnostic = "Invalid code point")
       parsed.diagnostics);
  let structure0 =
    Ast_mapper_to0.default_mapper.structure Ast_mapper_to0.default_mapper
      parsed.parsetree
  in
  ignore
    (Ast_mapper_from0.default_mapper.structure Ast_mapper_from0.default_mapper
       structure0)

(* Function-node attributes such as [@this] must stay node attributes across
   the v0 bridge: the built-in PPX reads decorators from [pexp_attributes],
   so a round trip that moves them into [p_attrs] silently disables them. *)
let test_fun_node_attrs_roundtrip_through_ast0 _ =
  let fun_expr =
    Ast_helper.Exp.fun_ ~loc
      ~attrs:[attr "this" (Parsetree.PStr [])]
      [
        Ast_helper.Exp.fun_param Asttypes.Nolabel
          (Ast_helper.Pat.var ~loc (Location.mknoloc "self"));
      ]
      (Ast_helper.Exp.ident ~loc (Location.mknoloc (Longident.Lident "self")))
  in
  let round_tripped = map_expr0 (map_expr_to0 fun_expr) in
  match round_tripped.pexp_desc with
  | Parsetree.Pexp_fun {params = [{p_attrs}]} ->
    OUnit.assert_equal ~msg:"node attributes survive on the function node"
      ["this"]
      (attr_names round_tripped.pexp_attributes);
    OUnit.assert_equal ~msg:"no attributes leak into p_attrs" []
      (attr_names p_attrs)
  | _ -> assert_failure "Expected a function after ast0 roundtrip"

(* The converse split: bridge-populated parameter attributes must come back
   as [p_attrs], not as function-node attributes. *)
let test_fun_param_attrs_roundtrip_through_ast0 _ =
  let fun_expr =
    Ast_helper.Exp.fun_ ~loc
      ~attrs:[attr "bar" (Parsetree.PStr [])]
      [
        Ast_helper.Exp.fun_param
          ~attrs:[attr "foo" (Parsetree.PStr [])]
          Asttypes.Nolabel
          (Ast_helper.Pat.var ~loc (Location.mknoloc "x"));
      ]
      (Ast_helper.Exp.ident ~loc (Location.mknoloc (Longident.Lident "x")))
  in
  let round_tripped = map_expr0 (map_expr_to0 fun_expr) in
  match round_tripped.pexp_desc with
  | Parsetree.Pexp_fun {params = [{p_attrs}]} ->
    OUnit.assert_equal ~msg:"node attributes stay on the node" ["bar"]
      (attr_names round_tripped.pexp_attributes);
    OUnit.assert_equal ~msg:"parameter attributes stay on the parameter" ["foo"]
      (attr_names p_attrs)
  | _ -> assert_failure "Expected a function after ast0 roundtrip"

let test_error_extension_backquoted_strings _ =
  let backquoted_string value =
    Ast_helper.Str.eval ~loc
      (Ast_helper.Exp.template ~loc [located_string value] [])
  in
  let extension =
    ( Location.mknoloc "error",
      Parsetree.PStr
        [
          backquoted_string {|plain\nmessage|};
          backquoted_string {|highlighted\nmessage|};
        ] )
  in
  let error = Builtin_attributes.error_of_extension extension in
  OUnit.assert_equal ~printer:(Printf.sprintf "%S") "plain\nmessage" error.msg;
  OUnit.assert_equal ~printer:(Printf.sprintf "%S") "highlighted\nmessage"
    error.if_highlight

let suites =
  __FILE__
  >::: [
         "public_record_rest_attr_is_not_internal"
         >:: test_public_record_rest_attr_is_not_internal;
         "fun_node_attrs_roundtrip_through_ast0"
         >:: test_fun_node_attrs_roundtrip_through_ast0;
         "fun_param_attrs_roundtrip_through_ast0"
         >:: test_fun_param_attrs_roundtrip_through_ast0;
         "ast0_strings_convert_to_internal_representation"
         >:: test_ast0_strings_convert_to_internal_representation;
         "ppx_byte_strings_convert_to_valid_utf8"
         >:: test_ppx_byte_strings_convert_to_valid_utf8;
         "string_literals_roundtrip_through_ast0"
         >:: test_string_literals_roundtrip_through_ast0;
         "raw_extension_payloads_roundtrip_through_ast0"
         >:: test_raw_extension_payloads_roundtrip_through_ast0;
         "tagged_templates_roundtrip_through_ast0"
         >:: test_tagged_templates_roundtrip_through_ast0;
         "ppx_rewritten_tagged_template_segments"
         >:: test_ppx_rewritten_tagged_template_segments;
         "ppx_rewritten_template_segments"
         >:: test_ppx_rewritten_template_segments;
         "interpolated_templates_roundtrip_through_ast0"
         >:: test_interpolated_templates_roundtrip_through_ast0;
         "ast0_json_interpolation_is_rejected"
         >:: test_ast0_json_interpolation_is_rejected;
         "string_source_reprints_after_ast0_roundtrip"
         >:: test_string_source_reprints_after_ast0_roundtrip;
         "invalid_utf8_doc_comment_roundtrips_through_ast0"
         >:: test_invalid_utf8_doc_comment_roundtrips_through_ast0;
         "malformed_internal_record_rest_attr_fails"
         >:: test_malformed_internal_record_rest_attr_fails;
         "record_rest_roundtrips_through_ast0"
         >:: test_record_rest_roundtrips_through_ast0;
         "constructor_args_roundtrip_through_ast0"
         >:: test_constructor_args_roundtrip_through_ast0;
         "constructor_args_keep_parentheses_location_in_ast0"
         >:: test_constructor_args_keep_parentheses_location_in_ast0;
         "constructor_argument_locations"
         >:: test_constructor_argument_locations;
         "constructor_normalization_keeps_argument_locations"
         >:: test_constructor_normalization_keeps_argument_locations;
         "incomplete_constructor_argument_locations"
         >:: test_incomplete_constructor_argument_locations;
         "fresh_ast0_constructor_tuple_reprints_without_internal_metadata"
         >:: test_fresh_ast0_constructor_tuple_reprints_without_internal_metadata;
         "ast0_explicit_arity_becomes_constructor_args"
         >:: test_ast0_explicit_arity_becomes_constructor_args;
         "fresh_ast0_constructor_tuple_defers_arity_to_typechecker"
         >:: test_fresh_ast0_constructor_tuple_defers_arity_to_typechecker;
         "polyvariant_args_roundtrip_through_ast0"
         >:: test_polyvariant_args_roundtrip_through_ast0;
         "polyvariant_args_keep_parentheses_location_in_ast0"
         >:: test_polyvariant_args_keep_parentheses_location_in_ast0;
         "value_constraint_roundtrips_through_ast0"
         >:: test_value_constraint_roundtrips_through_ast0;
         "function_cases_desugar_to_fun_match"
         >:: test_function_cases_desugar_to_fun_match;
         "error_extensions_accept_backquoted_strings"
         >:: test_error_extension_backquoted_strings;
         "field_runtime_name_reaches_ast0_as_an_attribute"
         >:: test_field_runtime_name_reaches_ast0_as_an_attribute;
         "field_runtime_name_keeps_its_place_on_the_wire"
         >:: test_field_runtime_name_keeps_its_place_on_the_wire;
         "field_runtime_name_keeps_ppx_order_on_the_wire"
         >:: test_field_runtime_name_keeps_ppx_order_on_the_wire;
         "constructor_runtime_tag_reaches_ast0_as_an_attribute"
         >:: test_constructor_runtime_tag_reaches_ast0_as_an_attribute;
       ]

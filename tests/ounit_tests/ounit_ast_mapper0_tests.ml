let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let assert_failure = OUnit.assert_failure

let loc = Location.none

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

let map_expr0 e =
  Ast_mapper_from0.default_mapper.expr Ast_mapper_from0.default_mapper e

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

(* Only a PPX can put an attribute on the function nested under a newtype
   (the parser attaches source attributes to the outer node), and the bridge
   deliberately preserves such attributes. The printer must not drop them
   when merging the newtype and the function into one parameter list. *)
let test_attributed_fun_under_newtype_prints_attribute _ =
  let fun_expr =
    Ast_helper.Exp.fun_ ~loc
      ~attrs:[attr "foo" (Parsetree.PStr [])]
      [
        Ast_helper.Exp.fun_param Asttypes.Nolabel
          (Ast_helper.Pat.var ~loc (Location.mknoloc "x"));
      ]
      (Ast_helper.Exp.ident ~loc (Location.mknoloc (Longident.Lident "x")))
  in
  let expr = Ast_helper.Exp.newtype ~loc (Location.mknoloc "t") fun_expr in
  let structure =
    [
      Ast_helper.Str.value ~loc Asttypes.Nonrecursive
        [
          Ast_helper.Vb.mk ~loc
            (Ast_helper.Pat.var ~loc (Location.mknoloc "f"))
            expr;
        ];
    ]
  in
  let printed =
    Res_printer.print_implementation ~width:80 structure ~comments:[]
  in
  OUnit.assert_bool "attribute on the fun under a newtype is printed"
    (Ext_string.contain_substring printed "@foo")

let map_expr_to0 e =
  Ast_mapper_to0.default_mapper.expr Ast_mapper_to0.default_mapper e

let attr_names attrs = List.map (fun ({Location.txt}, _) -> txt) attrs

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

let suites =
  __FILE__
  >::: [
         "public_record_rest_attr_is_not_internal"
         >:: test_public_record_rest_attr_is_not_internal;
         "attributed_fun_under_newtype_prints_attribute"
         >:: test_attributed_fun_under_newtype_prints_attribute;
         "fun_node_attrs_roundtrip_through_ast0"
         >:: test_fun_node_attrs_roundtrip_through_ast0;
         "fun_param_attrs_roundtrip_through_ast0"
         >:: test_fun_param_attrs_roundtrip_through_ast0;
         "malformed_internal_record_rest_attr_fails"
         >:: test_malformed_internal_record_rest_attr_fails;
         "record_rest_roundtrips_through_ast0"
         >:: test_record_rest_roundtrips_through_ast0;
         "function_cases_desugar_to_fun_match"
         >:: test_function_cases_desugar_to_fun_match;
       ]

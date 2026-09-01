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

let attr_names attrs = List.map (fun ({Location.txt}, _) -> txt) attrs

let assert_string_expr ~expected_source ~expected_semantic expr =
  match expr.Parsetree.pexp_desc with
  | Pexp_constant (Pconst_string {source; semantic}) ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected_source source;
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected_semantic semantic
  | _ -> assert_failure "Expected a string expression"

let assert_string_pat ~expected_source ~expected_semantic pat =
  match pat.Parsetree.ppat_desc with
  | Ppat_constant (Pconst_string {source; semantic}) ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected_source source;
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected_semantic semantic
  | _ -> assert_failure "Expected a string pattern"

let assert_template_expr ~expected expr =
  match expr.Parsetree.pexp_desc with
  | Pexp_template {source_segments = [actual]; values = []} ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected actual
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

let test_string_literals_roundtrip_through_ast0 _ =
  let semantic = "a\n😀" in
  let expr = Ast_helper.Exp.constant ~loc (Ast_helper.Const.string semantic) in
  assert_string_expr ~expected_source:{|a\n😀|} ~expected_semantic:semantic
    (map_expr0 (map_expr_to0 expr));
  let encoded = {|a\n\uD83D\uDE00|} in
  let template_expr = Ast_helper.Exp.template ~loc [encoded] [] in
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
  | Ppat_constant (Pconst_string {semantic; _}) ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") "a\n😀" semantic
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
      (Parsetree.Pconst_string {source = {|\x61|}; semantic = "a"})
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
  let tag =
    Ast_helper.Exp.ident ~loc (Location.mknoloc (Longident.Lident "tag"))
  in
  let value = Ast_helper.Exp.constant ~loc (Ast_helper.Const.integer "1") in
  let expression =
    Ast_helper.Exp.tagged_template ~loc
      ~attrs:[attr "keep" (Parsetree.PStr [])]
      tag [{|raw\unicode|}; " tail"] [value]
  in
  let expression0 = map_expr_to0 expression in
  OUnit.assert_bool "the frozen AST uses the tagged-template marker"
    (List.mem "res.taggedTemplate" (attr_names expression0.pexp_attributes));
  match (map_expr0 expression0).pexp_desc with
  | Pexp_tagged_template
      {
        tag = {pexp_desc = Pexp_ident {txt = Longident.Lident "tag"}};
        raw_sources;
        values = [{pexp_desc = Pexp_constant (Pconst_integer ("1", None))}];
      } ->
    OUnit.assert_equal ~printer:Ext_obj.dump [{|raw\unicode|}; " tail"]
      raw_sources;
    OUnit.assert_equal ["keep"]
      (attr_names (map_expr0 expression0).pexp_attributes)
  | _ -> assert_failure "Expected an explicit tagged template after roundtrip"

let test_interpolated_templates_roundtrip_through_ast0 _ =
  let value = Ast_helper.Exp.constant ~loc (Ast_helper.Const.integer "1") in
  let expression =
    Ast_helper.Exp.template ~loc
      ~attrs:[attr "keep" (Parsetree.PStr [])]
      [{|head\n|}; "tail"] [value]
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
      source_segments;
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
    Res_driver.parse_implementation_from_source ~for_printer:false
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
    Ast_helper.Str.eval ~loc (Ast_helper.Exp.template ~loc [value] [])
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
         "string_literals_roundtrip_through_ast0"
         >:: test_string_literals_roundtrip_through_ast0;
         "raw_extension_payloads_roundtrip_through_ast0"
         >:: test_raw_extension_payloads_roundtrip_through_ast0;
         "tagged_templates_roundtrip_through_ast0"
         >:: test_tagged_templates_roundtrip_through_ast0;
         "interpolated_templates_roundtrip_through_ast0"
         >:: test_interpolated_templates_roundtrip_through_ast0;
         "ast0_json_interpolation_is_rejected"
         >:: test_ast0_json_interpolation_is_rejected;
         "string_source_reprints_after_ast0_roundtrip"
         >:: test_string_source_reprints_after_ast0_roundtrip;
         "malformed_internal_record_rest_attr_fails"
         >:: test_malformed_internal_record_rest_attr_fails;
         "record_rest_roundtrips_through_ast0"
         >:: test_record_rest_roundtrips_through_ast0;
         "value_constraint_roundtrips_through_ast0"
         >:: test_value_constraint_roundtrips_through_ast0;
         "function_cases_desugar_to_fun_match"
         >:: test_function_cases_desugar_to_fun_match;
         "error_extensions_accept_backquoted_strings"
         >:: test_error_extension_backquoted_strings;
       ]

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
        arg_label = Nolabel;
        default = None;
        lhs = {ppat_desc = Ppat_var {txt = param}};
        rhs =
          {
            pexp_desc =
              Pexp_match ({pexp_desc = Pexp_ident {txt = Lident scrutinee}}, [_]);
          };
      } ->
    OUnit.assert_equal ~msg:"scrutinee is the introduced parameter" param
      scrutinee
  | _ -> assert_failure "Expected fun x -> match x with ... after desugaring"

let suites =
  __FILE__
  >::: [
         "public_record_rest_attr_is_not_internal"
         >:: test_public_record_rest_attr_is_not_internal;
         "malformed_internal_record_rest_attr_fails"
         >:: test_malformed_internal_record_rest_attr_fails;
         "record_rest_roundtrips_through_ast0"
         >:: test_record_rest_roundtrips_through_ast0;
         "function_cases_desugar_to_fun_match"
         >:: test_function_cases_desugar_to_fun_match;
       ]

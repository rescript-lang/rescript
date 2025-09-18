open Parsetree

let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let parse_structure source =
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:"Test.res" ~source
  in
  result.parsetree

let extract_fun_body structure =
  match structure with
  | [
   {
     Parsetree.pstr_desc =
       Pstr_value
         ( _,
           [{pvb_expr = {Parsetree.pexp_desc = Pexp_fun {rhs = body; _}; _}; _}]
         );
     _;
   };
  ] -> body
  | _ -> OUnit.assert_failure "unexpected structure for simple function"

let test_simple_return _ctx =
  let structure = parse_structure "let f = x => x + 1\n" in
  let body = extract_fun_body structure in
  OUnit.assert_bool "function body should be marked as a return"
    (Res_return_marker.expression_is_return body);
  match body.pexp_desc with
  | Pexp_apply {funct; args; _} ->
    OUnit.assert_bool "callee should not be marked"
      (not (Res_return_marker.expression_is_return funct));
    List.iter
      (fun (_, arg) ->
        OUnit.assert_bool "arguments should not be marked"
          (not (Res_return_marker.expression_is_return arg)))
      args
  | _ -> OUnit.assert_failure "expected application in function body"

let test_sequence_return _ctx =
  let structure = parse_structure "let f = x => {let y = x; y}\n" in
  let body = extract_fun_body structure in
  OUnit.assert_bool "function body should be marked"
    (Res_return_marker.expression_is_return body);
  match body.pexp_desc with
  | Pexp_sequence (first, second) ->
    OUnit.assert_bool "last expression in sequence should be marked"
      (Res_return_marker.expression_is_return second);
    OUnit.assert_bool "first expression in sequence should not be marked"
      (not (Res_return_marker.expression_is_return first))
  | Pexp_let (_, _, inner) ->
    OUnit.assert_bool "inner expression should be marked"
      (Res_return_marker.expression_is_return inner)
  | _ -> OUnit.assert_failure "expected sequence or let in body"

let test_switch_returns _ctx =
  let structure =
    parse_structure "let f = x => switch x { | 0 => 1 | _ => x }\n"
  in
  let body = extract_fun_body structure in
  OUnit.assert_bool "switch expression should be marked"
    (Res_return_marker.expression_is_return body);
  match body.pexp_desc with
  | Pexp_match (_, cases) ->
    List.iter
      (fun {pc_rhs; _} ->
        OUnit.assert_bool "case rhs should be marked"
          (Res_return_marker.expression_is_return pc_rhs))
      cases
  | _ -> OUnit.assert_failure "expected switch in function body"

let test_rewrite_loses_return_flag _ctx =
  let source = "let f = x => g(x)\n" in
  let structure = parse_structure source in
  let body = extract_fun_body structure in
  OUnit.assert_bool "pre-rewrite body should be marked"
    (Res_return_marker.expression_is_return body);
  let rewritten = Ppx_entry.rewrite_implementation structure in
  let rewritten_body = extract_fun_body rewritten in
  OUnit.assert_bool "rewritten body currently loses its return flag"
    (not (Res_return_marker.expression_is_return rewritten_body))

let suites =
  "return marker"
  >::: [
         "function body" >:: test_simple_return;
         "sequence tail" >:: test_sequence_return;
         "switch cases" >:: test_switch_returns;
         "rewrite loses return flag" >:: test_rewrite_loses_return_flag;
       ]

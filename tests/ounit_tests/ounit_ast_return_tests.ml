open Parsetree

let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let parse_structure source =
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:"Test.res" ~source
  in
  result.parsetree

let test_simple_return _ctx =
  let structure = parse_structure "let f = x => x + 1\n" in
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
  ] -> (
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
    | _ -> OUnit.assert_failure "expected application in function body")
  | _ -> OUnit.assert_failure "unexpected structure for simple function"

let test_sequence_return _ctx =
  let structure = parse_structure "let f = x => {let y = x; y}\n" in
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
  ] -> (
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
    | _ -> OUnit.assert_failure "expected sequence or let in body")
  | _ -> OUnit.assert_failure "unexpected structure for block body"

let test_switch_returns _ctx =
  let structure =
    parse_structure "let f = x => switch x { | 0 => 1 | _ => x }\n"
  in
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
  ] -> (
    OUnit.assert_bool "switch expression should be marked"
      (Res_return_marker.expression_is_return body);
    match body.pexp_desc with
    | Pexp_match (_, cases) ->
      List.iter
        (fun {pc_rhs; _} ->
          OUnit.assert_bool "case rhs should be marked"
            (Res_return_marker.expression_is_return pc_rhs))
        cases
    | _ -> OUnit.assert_failure "expected switch in function body")
  | _ -> OUnit.assert_failure "unexpected structure for switch body"

let suites =
  "return marker"
  >::: [
         "function body" >:: test_simple_return;
         "sequence tail" >:: test_sequence_return;
         "switch cases" >:: test_switch_returns;
       ]

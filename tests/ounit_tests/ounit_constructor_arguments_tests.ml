let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let assert_failure = OUnit.assert_failure

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
        Typemod.type_structure Env.initial_safe_string parsed.parsetree
          Location.none
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

let suites =
  __FILE__
  >::: [
         "constructor_argument_locations" >:: test_constructor_argument_locations;
         "constructor_normalization_keeps_argument_locations"
         >:: test_constructor_normalization_keeps_argument_locations;
         "incomplete_constructor_argument_locations"
         >:: test_incomplete_constructor_argument_locations;
       ]

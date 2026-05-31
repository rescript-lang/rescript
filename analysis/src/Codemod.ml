type transform_type = AddMissingCases

let rec collect_patterns p =
  match p.Parsetree.ppat_desc with
  | Ppat_or (p1, p2) -> collect_patterns p1 @ [p2]
  | _ -> [p]

let transform ~source ~pos ~debug ~typ ~hint =
  let structure, print_expr, _, _ = Xform.parse_implementation ~source in
  match typ with
  | AddMissingCases -> (
    let source = "let " ^ hint ^ " = ()" in
    let {Res_driver.parsetree = hint_structure} =
      Res_driver.parse_implementation_from_source ~for_printer:false
        ~display_filename:"<none>" ~source
    in
    match hint_structure with
    | [{pstr_desc = Pstr_value (_, [{pvb_pat = pattern}])}] -> (
      let cases =
        collect_patterns pattern
        |> List.map (fun (p : Parsetree.pattern) ->
               Ast_helper.Exp.case p (TypeUtils.Codegen.mk_fail_with_exp ()))
      in
      let result = ref None in
      let mk_iterator ~pos ~result =
        let expr (iterator : Ast_iterator.iterator) (exp : Parsetree.expression)
            =
          match exp.pexp_desc with
          | Pexp_match (e, existing_cases)
            when Pos.of_lexing exp.pexp_loc.loc_start = pos ->
            result :=
              Some {exp with pexp_desc = Pexp_match (e, existing_cases @ cases)}
          | _ -> Ast_iterator.default_iterator.expr iterator exp
        in
        {Ast_iterator.default_iterator with expr}
      in
      let iterator = mk_iterator ~pos ~result in
      iterator.structure iterator structure;
      match !result with
      | None ->
        if debug then print_endline "Found no result";
        exit 1
      | Some switch_expr ->
        print_expr ~range:(Loc.range_of_loc switch_expr.pexp_loc) switch_expr)
    | _ ->
      if debug then print_endline "Mismatch in expected structure";
      exit 1)

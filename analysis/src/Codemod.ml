type transformType = AddMissingCases

let rec collectPatterns p =
  match p.Parsetree.ppat_desc with
  | Ppat_or (p1, p2) -> collectPatterns p1 @ [p2]
  | _ -> [p]

let transform ~path ~pos ~debug ~typ ~hint =
  let structure, printExpr, _, _ = Xform.parseImplementation ~filename:path in
  match typ with
  | AddMissingCases -> (
    let source = "let " ^ hint ^ " = ()" in
    let {Res_driver.parsetree = hintStructure} =
      Res_driver.parse_implementation_from_source ~for_printer:false
        ~display_filename:"<none>" ~source
    in
    match hintStructure with
    | [{pstr_desc = Pstr_value (_, [{pvb_pat = pattern}])}] -> (
      let cases =
        collectPatterns pattern
        |> List.map (fun (p : Parsetree.pattern) ->
               Ast_helper.Exp.case p (TypeUtils.Codegen.mkFailWithExp ()))
      in
      let result = ref None in
      let mkIterator ~pos ~result =
        let expr (iterator : Ast_iterator.iterator) (exp : Parsetree.expression)
            =
          match exp.pexp_desc with
          | Pexp_match (e, existingCases)
            when Pos.ofLexing exp.pexp_loc.loc_start = pos ->
            result :=
              Some {exp with pexp_desc = Pexp_match (e, existingCases @ cases)}
          | _ -> Ast_iterator.default_iterator.expr iterator exp
        in
        {Ast_iterator.default_iterator with expr}
      in
      let iterator = mkIterator ~pos ~result in
      iterator.structure iterator structure;
      match !result with
      | None ->
        if debug then print_endline "Found no result";
        exit 1
      | Some switchExpr ->
        printExpr ~range:(Loc.rangeOfLoc switchExpr.pexp_loc) switchExpr)
    | _ ->
      if debug then print_endline "Mismatch in expected structure";
      exit 1)

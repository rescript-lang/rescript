let map_expr (mapper : Ast_mapper.mapper) (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_match (e, cases) ->
    let mapped_e = mapper.expr mapper e in
    let match_end_loc = expr.pexp_loc.loc_end in

    let is_ghost_case case =
      let open Parsetree in
      case.pc_lhs.ppat_loc.loc_ghost && case.pc_rhs.pexp_loc.loc_ghost
    in

    let rec process_cases mapped_cases cases =
      match cases with
      | [] -> mapped_cases
      | [last_case] when is_ghost_case last_case ->
        prerr_endline "last case";
        let mapped =
          mapper.case mapper
            {
              last_case with
              pc_loc = {last_case.pc_loc with loc_end = match_end_loc};
            }
        in
        process_cases (mapped :: mapped_cases) []
      | current :: (next :: _ as rest) when is_ghost_case current ->
        let mapped =
          mapper.case mapper
            {
              current with
              pc_loc =
                {current.pc_loc with loc_end = next.pc_lhs.ppat_loc.loc_start};
            }
        in
        process_cases (mapped :: mapped_cases) rest
      | c :: rest -> process_cases (mapper.case mapper c :: mapped_cases) rest
    in

    let adjusted_cases = process_cases [] cases in
    {expr with pexp_desc = Pexp_match (mapped_e, adjusted_cases)}
  | _ -> Ast_mapper.default_mapper.expr mapper expr

let map (tree : Parsetree.structure) =
  let mapper = {Ast_mapper.default_mapper with expr = map_expr} in
  mapper.structure mapper tree

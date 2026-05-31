let active ~config =
  (* When transitive reporting is off, the only dead modules would be empty modules *)
  config.DceConfig.run.transitive

let table = Hashtbl.create 1

let mark_dead ~config ~is_type ~loc path =
  if active ~config then
    let module_name = path |> DcePath.to_module_name ~is_type in
    match Hashtbl.find_opt table module_name with
    | Some _ -> ()
    | _ -> Hashtbl.replace table module_name (false, loc)

let mark_live ~config ~is_type ~(loc : Location.t) path =
  if active ~config then
    let module_name = path |> DcePath.to_module_name ~is_type in
    match Hashtbl.find_opt table module_name with
    | None -> Hashtbl.replace table module_name (true, loc)
    | Some (false, loc) -> Hashtbl.replace table module_name (true, loc)
    | Some (true, _) -> ()

(** Check if a module is dead and return issue if so. Pure - no logging. *)
let check_module_dead ~config ~file_name:pos_fname module_name : Issue.t option =
  if not (active ~config) then None
  else
    match Hashtbl.find_opt table module_name with
    | Some (false, loc) ->
      Hashtbl.remove table module_name;
      (* only report once *)
      let loc =
        if loc.loc_ghost then
          let pos =
            {Lexing.pos_fname; pos_lnum = 0; pos_bol = 0; pos_cnum = 0}
          in
          {Location.loc_start = pos; loc_end = pos; loc_ghost = false}
        else loc
      in
      Some (AnalysisResult.make_dead_module_issue ~loc ~module_name)
    | _ -> None

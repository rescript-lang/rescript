let active ~config =
  (* When transitive reporting is off, the only dead modules would be empty modules *)
  config.DceConfig.run.transitive

let table = Hashtbl.create 1

let markDead ~config ~isType ~loc path =
  if active ~config then
    let moduleName = path |> Common.Path.toModuleName ~isType in
    match Hashtbl.find_opt table moduleName with
    | Some _ -> ()
    | _ -> Hashtbl.replace table moduleName (false, loc)

let markLive ~config ~isType ~(loc : Location.t) path =
  if active ~config then
    let moduleName = path |> Common.Path.toModuleName ~isType in
    match Hashtbl.find_opt table moduleName with
    | None -> Hashtbl.replace table moduleName (true, loc)
    | Some (false, loc) -> Hashtbl.replace table moduleName (true, loc)
    | Some (true, _) -> ()

(** Check if a module is dead and return issue if so. Pure - no logging. *)
let checkModuleDead ~config ~fileName:pos_fname moduleName : Common.issue option
    =
  if not (active ~config) then None
  else
    match Hashtbl.find_opt table moduleName with
    | Some (false, loc) ->
      Hashtbl.remove table moduleName;
      (* only report once *)
      let loc =
        if loc.loc_ghost then
          let pos =
            {Lexing.pos_fname; pos_lnum = 0; pos_bol = 0; pos_cnum = 0}
          in
          {Location.loc_start = pos; loc_end = pos; loc_ghost = false}
        else loc
      in
      Some (AnalysisResult.make_dead_module_issue ~loc ~moduleName)
    | _ -> None

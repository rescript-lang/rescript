let active () =
  (* When transitive reporting is off, the only dead modules would be empty modules *)
  RunConfig.runConfig.transitive

let table = Hashtbl.create 1

let markDead ~isType ~loc path =
  if active () then
    let moduleName = path |> Common.Path.toModuleName ~isType in
    match Hashtbl.find_opt table moduleName with
    | Some _ -> ()
    | _ -> Hashtbl.replace table moduleName (false, loc)

let markLive ~isType ~(loc : Location.t) path =
  if active () then
    let moduleName = path |> Common.Path.toModuleName ~isType in
    match Hashtbl.find_opt table moduleName with
    | None -> Hashtbl.replace table moduleName (true, loc)
    | Some (false, loc) -> Hashtbl.replace table moduleName (true, loc)
    | Some (true, _) -> ()

let checkModuleDead ~fileName:pos_fname moduleName =
  if active () then
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
      Log_.warning ~loc
        (Common.DeadModule
           {
             message =
               Format.asprintf "@{<info>%s@} %s"
                 (moduleName |> Name.toInterface |> Name.toString)
                 "is a dead module as all its items are dead.";
           })
    | _ -> ()

open SharedTypes

let debug_references = ref true
let maybe_log m = if !debug_references then Log.log ("[ref] " ^ m)

let check_pos (line, char)
    {Location.loc_start = {pos_lnum; pos_bol; pos_cnum}; loc_end} =
  if line < pos_lnum || (line = pos_lnum && char < pos_cnum - pos_bol) then
    false
  else if
    line > loc_end.pos_lnum
    || (line = loc_end.pos_lnum && char > loc_end.pos_cnum - loc_end.pos_bol)
  then false
  else true

let loc_items_for_pos ~extra pos =
  extra.loc_items |> List.filter (fun {loc; loc_type = _} -> check_pos pos loc)

let line_col_to_cmt_loc ~pos:(line, col) = (line + 1, col)

let get_loc_item ~full ~pos ~debug =
  let log n msg = if debug then Printf.printf "getLocItem #%d: %s\n" n msg in
  let pos = line_col_to_cmt_loc ~pos in
  let loc_items = loc_items_for_pos ~extra:full.extra pos in
  if !Log.verbose then
    print_endline
      ("locItems:\n  "
      ^ (loc_items |> List.map loc_item_to_string |> String.concat "\n  "));
  let name_of li =
    match li.loc_type with
    | Typed (n, _, _) -> n
    | _ -> "NotFound"
  in
  match loc_items with
  | li1 :: li2 :: li3 :: ({loc_type = Typed ("makeProps", _, _)} as li4) :: _
    when full.file.uri |> Uri.is_interface ->
    log 1 "heuristic for makeProps in interface files";
    if debug then
      Printf.printf "n1:%s n2:%s n3:%s\n" (name_of li1) (name_of li2) (name_of li3);
    Some li4
  | [
   {loc_type = Constant _};
   ({loc_type = Typed ("createDOMElementVariadic", _, _)} as li2);
  ] ->
    log 3 "heuristic for <div>";
    Some li2
  | {loc_type = Typed ("makeProps", _, _)}
    :: ({loc_type = Typed ("make", _, _)} as li2)
    :: _ ->
    log 4
      "heuristic for </Comp> within fragments: take make as makeProps does not \
       work\n\
       the type is not great but jump to definition works";
    Some li2
  | [
   ({loc_type = Typed (_, _, LocalReference _)} as li1);
   ({loc_type = Typed (_, _, _)} as li2);
  ]
    when li1.loc = li2.loc ->
    log 5
      "heuristic for JSX and compiler combined:\n\
       ~x becomes Props#x\n\
       heuristic for: [Props, x], give loc of `x`";
    if debug then Printf.printf "n1:%s n2:%s\n" (name_of li1) (name_of li2);
    Some li2
  | [
   ({loc_type = Typed (_, _, LocalReference _)} as li1);
   ({loc_type = Typed (_, _, GlobalReference ("Js_OO", ["unsafe_downgrade"], _))}
    as li2);
   li3;
  ]
  (* For older compiler 9.0 or earlier *)
    when li1.loc = li2.loc && li2.loc = li3.loc ->
    (* Not currently testable on 9.1.4 *)
    log 6
      "heuristic for JSX and compiler combined:\n\
       ~x becomes Js_OO.unsafe_downgrade(Props)#x\n\
       heuristic for: [Props, unsafe_downgrade, x], give loc of `x`";
    Some li3
  | [
   ({loc_type = Typed (_, _, LocalReference (_, Value))} as li1);
   ({loc_type = Typed (_, _, Definition (_, Value))} as li2);
  ] ->
    log 7
      "heuristic for JSX on type-annotated labeled (~arg:t):\n\
       (~arg:t) becomes Props#arg\n\
       Props has the location range of arg:t\n\
       arg has the location range of arg\n\
       heuristic for: [Props, arg], give loc of `arg`";
    if debug then Printf.printf "n1:%s n2:%s\n" (name_of li1) (name_of li2);
    Some li2
  | [li1; li2; li3] when li1.loc = li2.loc && li2.loc = li3.loc ->
    (* Not currently testable on 9.1.4 *)
    log 8
      "heuristic for JSX with at most one child\n\
       heuristic for: [makeProps, make, createElement], give the loc of `make` ";
    Some li2
  | [li1; li2; li3; li4]
    when li1.loc = li2.loc && li2.loc = li3.loc && li3.loc = li4.loc ->
    log 9
      "heuristic for JSX variadic, e.g. <C> {x} {y} </C>\n\
       heuristic for: [React.null, makeProps, make, createElementVariadic], \
       give the loc of `make`";
    if debug then
      Printf.printf "n1:%s n2:%s n3:%s n4:%s\n" (name_of li1) (name_of li2)
        (name_of li3) (name_of li4);
    Some li3
  | {loc_type = Typed (_, {desc = Tconstr (path, _, _)}, _)} :: li :: _
    when Utils.is_uncurried_internal path ->
    Some li
  | li :: _ -> Some li
  | _ -> None

let declared_for_tip ~(stamps : Stamps.t) stamp (tip : Tip.t) =
  match tip with
  | Value ->
    Stamps.find_value stamps stamp
    |> Option.map (fun x -> {x with Declared.item = ()})
  | Field _ | Constructor _ | Type ->
    Stamps.find_type stamps stamp
    |> Option.map (fun x -> {x with Declared.item = ()})
  | Module ->
    Stamps.find_module stamps stamp
    |> Option.map (fun x -> {x with Declared.item = ()})

let get_field (file : File.t) stamp name =
  match Stamps.find_type file.stamps stamp with
  | None -> None
  | Some {item = {kind}} -> (
    match kind with
    | Record fields -> fields |> List.find_opt (fun f -> f.fname.txt = name)
    | _ -> None)

let get_constructor (file : File.t) stamp name =
  match Stamps.find_type file.stamps stamp with
  | None -> None
  | Some {item = {kind}} -> (
    match kind with
    | Variant constructors -> (
      match
        constructors
        |> List.find_opt (fun const -> const.Constructor.cname.txt = name)
      with
      | None -> None
      | Some const -> Some const)
    | _ -> None)

let exported_for_tip ~env ~path ~package ~(tip : Tip.t) =
  match ResolvePath.resolve_path ~env ~path ~package with
  | None ->
    Log.log ("Cannot resolve path " ^ path_to_string path);
    None
  | Some (env, name) -> (
    let kind =
      match tip with
      | Value -> Exported.Value
      | Field _ | Constructor _ | Type -> Exported.Type
      | Module -> Exported.Module
    in
    match Exported.find env.exported kind name with
    | None ->
      Log.log ("Exported not found for tip " ^ name ^ " > " ^ Tip.to_string tip);
      None
    | Some stamp -> Some (env, name, stamp))

let defined_for_loc ~file ~package loc_kind =
  let inner ~file stamp (tip : Tip.t) =
    match tip with
    | Constructor name -> (
      match get_constructor file stamp name with
      | None -> None
      | Some constructor ->
        Some (constructor.docstring, `Constructor constructor))
    | Field name ->
      Some
        ( (match get_field file stamp name with
          | None -> []
          | Some field -> field.docstring),
          `Field )
    | _ -> (
      maybe_log
        ("Trying for declared " ^ Tip.to_string tip ^ " " ^ string_of_int stamp
       ^ " in file " ^ Uri.to_string file.uri);
      match declared_for_tip ~stamps:file.stamps stamp tip with
      | None -> None
      | Some declared -> Some (declared.docstring, `Declared))
  in
  match loc_kind with
  | NotFound -> None
  | LocalReference (stamp, tip) | Definition (stamp, tip) ->
    inner ~file stamp tip
  | GlobalReference (module_name, path, tip) -> (
    maybe_log ("Getting global " ^ module_name);
    match ProcessCmt.file_for_module ~package module_name with
    | None ->
      Log.log ("Cannot get module " ^ module_name);
      None
    | Some file -> (
      let env = QueryEnv.from_file file in
      match exported_for_tip ~env ~path ~package ~tip with
      | None -> None
      | Some (env, name, stamp) -> (
        maybe_log ("Getting for " ^ string_of_int stamp ^ " in " ^ name);
        match inner ~file:env.file stamp tip with
        | None ->
          Log.log "could not get defined";
          None
        | Some res ->
          maybe_log "Yes!! got it";
          Some res)))

(** Find alternative declaration: from res in case of interface, or from resi in case of implementation  *)
let alternate_declared ~(file : File.t) ~package (declared : _ Declared.t) tip =
  match Hashtbl.find_opt package.paths_for_module file.module_name with
  | None -> None
  | Some paths -> (
    match paths with
    | IntfAndImpl {resi; res} -> (
      maybe_log
        ("alternateDeclared for " ^ file.module_name ^ " has both resi and res");
      let alternate_uri = if Uri.is_interface file.uri then res else resi in
      match Cmt.full_from_uri ~uri:(Uri.from_path alternate_uri) with
      | None -> None
      | Some {file; extra} -> (
        let env = QueryEnv.from_file file in
        let path = ModulePath.to_path declared.module_path declared.name.txt in
        maybe_log ("find declared for path " ^ path_to_string path);
        let declared_opt =
          match exported_for_tip ~env ~path ~package ~tip with
          | None -> None
          | Some (_env, _name, stamp) ->
            declared_for_tip ~stamps:file.stamps stamp tip
        in
        match declared_opt with
        | None -> None
        | Some declared -> Some (file, extra, declared)))
    | _ ->
      maybe_log ("alternateDeclared for " ^ file.module_name ^ " not found");

      None)

let rec resolve_module_reference ?(paths_seen = []) ~file ~package
    (declared : Module.t Declared.t) =
  match declared.item with
  | Structure _ -> Some (file, Some declared)
  | Constraint (_moduleItem, module_type_item) ->
    resolve_module_reference ~paths_seen ~file ~package
      {declared with item = module_type_item}
  | Ident path -> (
    let env = QueryEnv.from_file file in
    match ResolvePath.from_compiler_path ~env path with
    | NotFound -> None
    | Exported (env, name) -> (
      match Exported.find env.exported Exported.Module name with
      | None -> None
      | Some stamp -> (
        match Stamps.find_module env.file.stamps stamp with
        | None -> None
        | Some md -> Some (env.file, Some md)))
    | Global (module_name, path) -> (
      match ProcessCmt.file_for_module ~package module_name with
      | None -> None
      | Some file -> (
        let env = QueryEnv.from_file file in
        match ResolvePath.resolve_path ~env ~package ~path with
        | None -> None
        | Some (env, name) -> (
          match Exported.find env.exported Exported.Module name with
          | None -> None
          | Some stamp -> (
            match Stamps.find_module env.file.stamps stamp with
            | None -> None
            | Some md -> Some (env.file, Some md)))))
    | Stamp stamp -> (
      match Stamps.find_module file.stamps stamp with
      | None -> None
      | Some ({item = Ident path} as md) when not (List.mem path paths_seen) ->
        (* avoid possible infinite loops *)
        resolve_module_reference ~file ~package ~paths_seen:(path :: paths_seen) md
      | Some md -> Some (file, Some md))
    | GlobalMod name -> (
      match ProcessCmt.file_for_module ~package name with
      | None -> None
      | Some file -> Some (file, None)))

let validate_loc (loc : Location.t) (backup : Location.t) =
  if loc.loc_start.pos_cnum = -1 then
    if backup.loc_start.pos_cnum = -1 then
      {
        Location.loc_ghost = true;
        loc_start = {pos_cnum = 0; pos_lnum = 1; pos_bol = 0; pos_fname = ""};
        loc_end = {pos_cnum = 0; pos_lnum = 1; pos_bol = 0; pos_fname = ""};
      }
    else backup
  else loc

let resolve_module_definition ~(file : File.t) ~package stamp =
  match Stamps.find_module file.stamps stamp with
  | None -> None
  | Some md -> (
    match resolve_module_reference ~file ~package md with
    | None -> None
    | Some (file, declared) ->
      let loc =
        match declared with
        | None -> Uri.to_top_level_loc file.uri
        | Some declared -> validate_loc declared.name.loc declared.extent_loc
      in
      Some (file.uri, loc))

let definition ~file ~package stamp (tip : Tip.t) =
  match tip with
  | Constructor name -> (
    match get_constructor file stamp name with
    | None -> None
    | Some constructor -> Some (file.uri, constructor.cname.loc))
  | Field name -> (
    match get_field file stamp name with
    | None -> None
    | Some field -> Some (file.uri, field.fname.loc))
  | Module -> resolve_module_definition ~file ~package stamp
  | _ -> (
    match declared_for_tip ~stamps:file.stamps stamp tip with
    | None -> None
    | Some declared ->
      let file_impl, declared_impl =
        match alternate_declared ~package ~file declared tip with
        | Some (file_impl, _extra, declared_impl) when Uri.is_interface file.uri ->
          (file_impl, declared_impl)
        | _ -> (file, declared)
      in
      let loc = validate_loc declared_impl.name.loc declared_impl.extent_loc in
      let env = QueryEnv.from_file file_impl in
      let uri =
        ResolvePath.get_source_uri ~env ~package declared_impl.module_path
      in
      maybe_log ("Inner uri " ^ Uri.to_string uri);
      Some (uri, loc))

let definition_for_loc_item ~full:{file; package} loc_item =
  match loc_item.loc_type with
  | Typed (_, _, Definition (stamp, tip)) -> (
    maybe_log
      ("Typed Definition stamp:" ^ string_of_int stamp ^ " tip:"
     ^ Tip.to_string tip);
    match declared_for_tip ~stamps:file.stamps stamp tip with
    | None -> None
    | Some declared ->
      maybe_log ("Declared " ^ declared.name.txt);
      if declared.is_exported then (
        maybe_log ("exported, looking for alternate " ^ file.module_name);
        match alternate_declared ~package ~file declared tip with
        | None -> None
        | Some (file, _extra, declared) ->
          let loc = validate_loc declared.name.loc declared.extent_loc in
          Some (file.uri, loc))
      else None)
  | Typed (_, _, NotFound)
  | LModule (NotFound | Definition (_, _))
  | TypeDefinition (_, _, _)
  | Constant _ ->
    None
  | TopLevelModule name -> (
    maybe_log ("Toplevel " ^ name);
    match Hashtbl.find_opt package.paths_for_module name with
    | None -> None
    | Some paths ->
      let uri = get_uri paths in
      Some (uri, Uri.to_top_level_loc uri))
  | LModule (LocalReference (stamp, tip))
  | Typed (_, _, LocalReference (stamp, tip)) ->
    maybe_log ("Local defn " ^ Tip.to_string tip);
    definition ~file ~package stamp tip
  | LModule (GlobalReference (module_name, path, tip))
  | Typed (_, _, GlobalReference (module_name, path, tip)) -> (
    maybe_log
      ("Typed GlobalReference moduleName:" ^ module_name ^ " path:"
     ^ path_to_string path ^ " tip:" ^ Tip.to_string tip);
    match ProcessCmt.file_for_module ~package module_name with
    | None -> None
    | Some file -> (
      let env = QueryEnv.from_file file in
      match exported_for_tip ~env ~path ~package ~tip with
      | None -> None
      | Some (env, _name, stamp) ->
        (* oooh wht do I do if the stamp is inside a pseudo-file? *)
        maybe_log ("Got stamp " ^ string_of_int stamp);
        definition ~file:env.file ~package stamp tip))

let dig_constructor ~env ~package path =
  match ResolvePath.resolve_from_compiler_path ~env ~package path with
  | NotFound -> None
  | Stamp stamp -> (
    match Stamps.find_type env.file.stamps stamp with
    | None -> None
    | Some t -> Some (env, t))
  | Exported (env, name) -> (
    match Exported.find env.exported Exported.Type name with
    | None -> None
    | Some stamp -> (
      match Stamps.find_type env.file.stamps stamp with
      | None -> None
      | Some t -> Some (env, t)))
  | _ -> None

let type_definition_for_loc_item ~full:{file; package} loc_item =
  match loc_item.loc_type with
  | Constant _ | TopLevelModule _ | LModule _ -> None
  | TypeDefinition _ -> Some (file.uri, loc_item.loc)
  | Typed (_, typ, _) -> (
    let env = QueryEnv.from_file file in
    match Shared.dig_constructor typ with
    | None -> None
    | Some path -> (
      match dig_constructor ~env ~package path with
      | Some (env, declared) -> Some (env.file.uri, declared.item.decl.type_loc)
      | None -> None))

let is_visible (declared : _ Declared.t) =
  declared.is_exported
  &&
  let rec loop (v : ModulePath.t) =
    match v with
    | File _ -> true
    | NotVisible -> false
    | IncludedModule (_, inner) -> loop inner
    | ExportedModule {module_path = inner} -> loop inner
  in
  loop declared.module_path

type references = {
  uri: Uri.t;
  loc_opt: Location.t option; (* None: reference to a toplevel module *)
}

let for_local_stamp ~full:{file; extra; package} stamp (tip : Tip.t) =
  let env = QueryEnv.from_file file in
  match
    match tip with
    | Constructor name ->
      get_constructor file stamp name
      |> Option.map (fun x -> x.Constructor.stamp)
    | Field name -> get_field file stamp name |> Option.map (fun x -> x.stamp)
    | _ -> Some stamp
  with
  | None -> []
  | Some local_stamp -> (
    match Hashtbl.find_opt extra.internal_references local_stamp with
    | None -> []
    | Some locs ->
      maybe_log ("Checking externals: " ^ string_of_int stamp);
      let externals =
        match declared_for_tip ~stamps:env.file.stamps stamp tip with
        | None -> []
        | Some declared ->
          if is_visible declared then (
            let alternative_references =
              match alternate_declared ~package ~file declared tip with
              | None -> []
              | Some (file, extra, {stamp}) -> (
                match
                  match tip with
                  | Constructor name ->
                    get_constructor file stamp name
                    |> Option.map (fun x -> x.Constructor.stamp)
                  | Field name ->
                    get_field file stamp name |> Option.map (fun x -> x.stamp)
                  | _ -> Some stamp
                with
                | None -> []
                | Some local_stamp -> (
                  match
                    Hashtbl.find_opt extra.internal_references local_stamp
                  with
                  | None -> []
                  | Some locs ->
                    locs
                    |> List.map (fun loc -> {uri = file.uri; loc_opt = Some loc})
                  ))
              (* if this file has a corresponding interface or implementation file
                 also find the references in that file *)
            in
            let path =
              ModulePath.to_path declared.module_path declared.name.txt
            in
            maybe_log ("Now checking path " ^ path_to_string path);
            let this_module_name = file.module_name in
            let externals =
              package.project_files |> FileSet.elements
              |> List.filter (fun name -> name <> file.module_name)
              |> List.map (fun module_name ->
                     Cmt.fulls_from_module ~package ~module_name
                     |> List.map (fun {file; extra} ->
                            match
                              Hashtbl.find_opt extra.external_references
                                this_module_name
                            with
                            | None -> []
                            | Some refs ->
                              let locs =
                                refs
                                |> Utils.filter_map (fun (p, t, locs) ->
                                       if p = path && t = tip then Some locs
                                       else None)
                              in
                              locs
                              |> List.map (fun loc ->
                                     {uri = file.uri; loc_opt = Some loc})))
              |> List.concat |> List.concat
            in
            alternative_references @ externals)
          else (
            maybe_log "Not visible";
            [])
      in
      List.append
        (locs |> List.map (fun loc -> {uri = file.uri; loc_opt = Some loc}))
        externals)

let all_references_for_loc_item ~full:({file; package} as full) loc_item =
  match loc_item.loc_type with
  | TopLevelModule module_name ->
    let other_modules_references =
      package.project_files |> FileSet.elements
      |> Utils.filter_map (fun name ->
             match ProcessCmt.file_for_module ~package name with
             | None -> None
             | Some file -> Cmt.full_from_uri ~uri:file.uri)
      |> List.map (fun full ->
             match Hashtbl.find_opt full.extra.file_references module_name with
             | None -> []
             | Some locs ->
               locs |> LocationSet.elements
               |> List.map (fun loc ->
                      {
                        uri = Uri.from_path loc.Location.loc_start.pos_fname;
                        loc_opt = Some loc;
                      }))
      |> List.flatten
    in
    let target_module_references =
      match Hashtbl.find_opt package.paths_for_module module_name with
      | None -> []
      | Some paths ->
        let module_src_to_ref src = {uri = Uri.from_path src; loc_opt = None} in
        get_src paths |> List.map module_src_to_ref
    in
    List.append target_module_references other_modules_references
  | Typed (_, _, NotFound) | LModule NotFound | Constant _ -> []
  | TypeDefinition (_, _, stamp) -> for_local_stamp ~full stamp Type
  | Typed (_, _, (LocalReference (stamp, tip) | Definition (stamp, tip)))
  | LModule (LocalReference (stamp, tip) | Definition (stamp, tip)) ->
    maybe_log
      ("Finding references for " ^ Uri.to_string file.uri ^ " and stamp "
     ^ string_of_int stamp ^ " and tip " ^ Tip.to_string tip);
    for_local_stamp ~full stamp tip
  | LModule (GlobalReference (module_name, path, tip))
  | Typed (_, _, GlobalReference (module_name, path, tip)) -> (
    match ProcessCmt.file_for_module ~package module_name with
    | None -> []
    | Some file -> (
      let env = QueryEnv.from_file file in
      match exported_for_tip ~env ~path ~package ~tip with
      | None -> []
      | Some (env, _name, stamp) -> (
        match Cmt.full_from_uri ~uri:env.file.uri with
        | None -> []
        | Some full ->
          maybe_log
            ("Finding references for (global) " ^ Uri.to_string env.file.uri
           ^ " and stamp " ^ string_of_int stamp ^ " and tip "
           ^ Tip.to_string tip);
          for_local_stamp ~full stamp tip)))

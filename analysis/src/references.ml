open Shared_types

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

(** External references in namespaced projects are indexed by the public
  * namespace path, e.g. MyNamespace.MyModule1.myFunc1, while definitions live
  * in hidden compiled modules like MyModule1-MyNamespace.
  * We return the lookup key pair used by the external reference index.
  *)
let normalize_external_reference_key ~namespace ~module_name ~path =
  match namespace with
  | Some namespace when String.ends_with ~suffix:("-" ^ namespace) module_name
    ->
    let suffix_len = String.length namespace + 1 in
    let source_module_len = String.length module_name - suffix_len in
    let source_module = String.sub module_name 0 source_module_len in
    (namespace, source_module :: path)
  | _ -> (module_name, path)

let get_loc_item ~full ~pos ~debug =
  let log n msg = if debug then Printf.printf "get_loc_item #%d: %s\n" n msg in
  let pos = line_col_to_cmt_loc ~pos in
  let loc_items = loc_items_for_pos ~extra:full.extra pos in
  if !Log.verbose then
    print_endline
      ("loc_items:\n  "
      ^ (loc_items |> List.map loc_item_to_string |> String.concat "\n  "));
  let name_of li =
    match li.loc_type with
    | Typed (n, _, _) -> n
    | _ -> "NotFound"
  in
  match loc_items with
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

let exported_for_tip ~state ~env ~path ~package ~(tip : Tip.t) =
  match Resolve_path.resolve_path ~state ~env ~path ~package with
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

let defined_for_loc ~state ~file ~package loc_kind =
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
    match Process_cmt.file_for_module ~state ~package module_name with
    | None ->
      Log.log ("Cannot get module " ^ module_name);
      None
    | Some file -> (
      let env = Query_env.from_file file in
      match exported_for_tip ~state ~env ~path ~package ~tip with
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
let alternate_declared ~state ~(file : File.t) ~package
    (declared : _ Declared.t) tip =
  match Hashtbl.find_opt package.paths_for_module file.module_name with
  | None -> None
  | Some paths -> (
    match paths with
    | IntfAndImpl {resi; res} -> (
      maybe_log
        ("alternate_declared for " ^ file.module_name ^ " has both resi and res");
      let alternate_uri = if Uri.is_interface file.uri then res else resi in
      match Cmt.full_from_uri ~state ~uri:(Uri.from_path alternate_uri) with
      | None -> None
      | Some {file; extra} -> (
        let env = Query_env.from_file file in
        let path = Module_path.to_path declared.module_path declared.name.txt in
        maybe_log ("find declared for path " ^ path_to_string path);
        let declared_opt =
          match exported_for_tip ~state ~env ~path ~package ~tip with
          | None -> None
          | Some (_env, _name, stamp) ->
            declared_for_tip ~stamps:file.stamps stamp tip
        in
        match declared_opt with
        | None -> None
        | Some declared -> Some (file, extra, declared)))
    | _ ->
      maybe_log ("alternate_declared for " ^ file.module_name ^ " not found");

      None)

let rec resolve_module_reference ?(paths_seen = []) ~state ~file ~package
    (declared : Module.t Declared.t) =
  match declared.item with
  | Structure _ -> Some (file, Some declared)
  | Constraint (_moduleItem, module_type_item) ->
    resolve_module_reference ~paths_seen ~state ~file ~package
      {declared with item = module_type_item}
  | Ident path -> (
    let env = Query_env.from_file file in
    match Resolve_path.from_compiler_path ~env path with
    | NotFound -> None
    | Exported (env, name) -> (
      match Exported.find env.exported Exported.Module name with
      | None -> None
      | Some stamp -> (
        match Stamps.find_module env.file.stamps stamp with
        | None -> None
        | Some md -> Some (env.file, Some md)))
    | Global (module_name, path) -> (
      match Process_cmt.file_for_module ~state ~package module_name with
      | None -> None
      | Some file -> (
        let env = Query_env.from_file file in
        match Resolve_path.resolve_path ~state ~env ~package ~path with
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
        resolve_module_reference ~state ~file ~package
          ~paths_seen:(path :: paths_seen) md
      | Some md -> Some (file, Some md))
    | GlobalMod name -> (
      match Process_cmt.file_for_module ~state ~package name with
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

let resolve_module_definition ~state ~(file : File.t) ~package stamp =
  match Stamps.find_module file.stamps stamp with
  | None -> None
  | Some md -> (
    match resolve_module_reference ~state ~file ~package md with
    | None -> None
    | Some (file, declared) ->
      let loc =
        match declared with
        | None -> Uri.to_top_level_loc file.uri
        | Some declared -> validate_loc declared.name.loc declared.extent_loc
      in
      Some (file.uri, loc))

let rec definition ~state ~(file : File.t) ~package stamp (tip : Tip.t) =
  match tip with
  | Constructor name ->
    get_constructor file stamp name
    |> Option.map (fun (constructor : Constructor.t) ->
        (file.uri, constructor.cname.loc))
    |> label_definition ~state ~file ~package stamp tip
  | Field name ->
    get_field file stamp name
    |> Option.map (fun field -> (file.uri, field.fname.loc))
    |> label_definition ~state ~file ~package stamp tip
  | Module -> resolve_module_definition ~state ~file ~package stamp
  | _ -> (
    match declared_for_tip ~stamps:file.stamps stamp tip with
    | None -> None
    | Some declared ->
      let file_impl, declared_impl =
        match alternate_declared ~state ~package ~file declared tip with
        | Some (file_impl, _extra, declared_impl) when Uri.is_interface file.uri
          ->
          (file_impl, declared_impl)
        | _ -> (file, declared)
      in
      let loc = validate_loc declared_impl.name.loc declared_impl.extent_loc in
      let env = Query_env.from_file file_impl in
      let uri =
        Resolve_path.get_source_uri ~state ~env ~package
          declared_impl.module_path
      in
      maybe_log ("Inner uri " ^ Uri.to_string uri);
      Some (uri, loc))

(* The constructors and fields of an included type have no location: use
   those of the type it copies, and [own] for any other type. *)
and label_definition ~state ~file ~package stamp tip own =
  match Process_extra.include_origin ~file stamp tip with
  | Origin origin when origin <> stamp ->
    definition ~state ~file ~package origin tip
  | Other_file (module_name, path) ->
    global_definition ~state ~package module_name path tip
  | Origin _ -> own

and global_definition ~state ~package module_name path tip =
  match Process_cmt.file_for_module ~state ~package module_name with
  | None -> None
  | Some file -> (
    let env = Query_env.from_file file in
    match exported_for_tip ~state ~env ~path ~package ~tip with
    | None -> None
    | Some (env, _name, stamp) ->
      (* oooh wht do I do if the stamp is inside a pseudo-file? *)
      maybe_log ("Got stamp " ^ string_of_int stamp);
      definition ~state ~file:env.file ~package stamp tip)

let definition_for_loc_item ~state ~full:{file; package} loc_item =
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
        match alternate_declared ~state ~package ~file declared tip with
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
    definition ~state ~file ~package stamp tip
  | LModule (GlobalReference (module_name, path, tip))
  | Typed (_, _, GlobalReference (module_name, path, tip)) ->
    maybe_log
      ("Typed GlobalReference module_name:" ^ module_name ^ " path:"
     ^ path_to_string path ^ " tip:" ^ Tip.to_string tip);
    global_definition ~state ~package module_name path tip

let dig_constructor ~state ~env ~package path =
  match Resolve_path.resolve_from_compiler_path ~state ~env ~package path with
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

let type_definition_for_loc_item ~state ~full:{file; package} loc_item =
  match loc_item.loc_type with
  | Constant _ | TopLevelModule _ | LModule _ -> None
  | TypeDefinition _ -> Some (file.uri, loc_item.loc)
  | Typed (_, typ, _) -> (
    let env = Query_env.from_file file in
    match Shared.dig_constructor typ with
    | None -> None
    | Some path -> (
      match dig_constructor ~state ~env ~package path with
      | Some (env, declared) ->
        let uri =
          Resolve_path.get_source_uri ~state ~env ~package declared.module_path
        in
        Some (uri, declared.item.decl.type_loc)
      | None -> None))

let is_visible (declared : _ Declared.t) =
  declared.is_exported
  &&
  let rec loop (v : Module_path.t) =
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

(* The stamps that stand for the same declaration as [stamp]: the member it
   was included from, when that is in this file, and all included copies of
   that member. For a constructor or field, these are stamps of its type. *)
let include_family ~(file : File.t) stamp (tip : Tip.t) =
  let origin = Process_extra.include_origin ~file stamp tip in
  let copies = ref [stamp] in
  let add_copy copy (declared : _ Declared.t) =
    match Module_path.included_path declared.module_path declared.name.txt with
    | Some _ ->
      if Process_extra.include_origin ~file copy tip = origin then
        copies := copy :: !copies
    | None -> ()
  in
  (match tip with
  | Value -> Stamps.iter_values add_copy file.stamps
  | Module -> Stamps.iter_modules add_copy file.stamps
  | Type | Field _ | Constructor _ -> Stamps.iter_types add_copy file.stamps);
  let copies = List.sort_uniq compare !copies in
  let family =
    match origin with
    | Origin origin -> origin :: List.filter (( <> ) origin) copies
    | Other_file _ -> copies
  in
  (origin, family)

(* A module and path under which other files record their uses of a member,
   and the module whose own file is not searched. *)
type external_key = {owner: string; module_name: string; path: path; tip: Tip.t}

(* The stamp under which [file] records the references to [tip]: for a
   constructor or field, [stamp] is the stamp of its type. *)
let tip_stamp ~(file : File.t) stamp (tip : Tip.t) =
  match tip with
  | Constructor name ->
    get_constructor file stamp name |> Option.map (fun x -> x.Constructor.stamp)
  | Field name -> get_field file stamp name |> Option.map (fun x -> x.stamp)
  | Value | Type | Module -> Some stamp

(* The references to [stamp] in its file and in the other file of the same
   module (.res or .resi), and the key of its uses in other files. *)
let references_in_file ~state ~full:{file; extra; package} stamp (tip : Tip.t) =
  match tip_stamp ~file stamp tip with
  | None -> ([], [])
  | Some local_stamp -> (
    let references_of ~(file : File.t) ~extra local_stamp =
      Hashtbl.find_opt extra.internal_references local_stamp
      |> Option.value ~default:[]
      |> List.map (fun loc -> {uri = file.uri; loc_opt = Some loc})
    in
    let locs = references_of ~file ~extra local_stamp in
    match declared_for_tip ~stamps:file.stamps stamp tip with
    | Some declared when is_visible declared ->
      let alternative_references =
        match alternate_declared ~state ~package ~file declared tip with
        | None -> []
        | Some (file, extra, {stamp}) -> (
          match tip_stamp ~file stamp tip with
          | None -> []
          | Some local_stamp -> references_of ~file ~extra local_stamp)
      in
      let path = Module_path.to_path declared.module_path declared.name.txt in
      maybe_log ("Now checking path " ^ path_to_string path);
      let module_name, path =
        normalize_external_reference_key ~namespace:package.namespace
          ~module_name:file.module_name ~path
      in
      ( locs @ alternative_references,
        [{owner = file.module_name; module_name; path; tip}] )
    | Some _ | None ->
      maybe_log "Not visible";
      (locs, []))

(* The uses that other files record under [keys], found in one pass over the
   project. *)
let external_references ~package keys =
  package.project_files |> File_set.elements
  |> List.concat_map (fun module_name ->
      match List.filter (fun key -> key.owner <> module_name) keys with
      | [] -> []
      | keys ->
        Cmt.fulls_from_module ~package ~module_name
        |> List.concat_map (fun {file; extra} ->
            keys
            |> List.concat_map (fun key ->
                match
                  Hashtbl.find_opt extra.external_references key.module_name
                with
                | None -> []
                | Some refs ->
                  refs
                  |> List.filter_map (fun (p, t, loc) ->
                      if p = key.path && t = key.tip then Some loc else None)
                  |> List.map (fun loc -> {uri = file.uri; loc_opt = Some loc}))))

(* The references to [stamp], its included copies and the member they copy
   that are found in their own files, and the keys of their uses in other
   files. A member copied from another file is looked up there. *)
let rec family_references ~state ~full stamp tip =
  let origin, family = include_family ~file:full.file stamp tip in
  let locs, keys =
    family
    |> List.map (fun stamp -> references_in_file ~state ~full stamp tip)
    |> List.split
  in
  let locs = List.concat locs and keys = List.concat keys in
  match origin with
  | Origin _ -> (locs, keys)
  | Other_file (module_name, path) ->
    let source_locs, source_keys =
      global_family_references ~state ~package:full.package module_name path tip
    in
    (locs @ source_locs, keys @ source_keys)

and global_family_references ~state ~package module_name path tip =
  match Process_cmt.file_for_module ~state ~package module_name with
  | None -> ([], [])
  | Some file -> (
    let env = Query_env.from_file file in
    match exported_for_tip ~state ~env ~path ~package ~tip with
    | None -> ([], [])
    | Some (env, _name, stamp) -> (
      match Cmt.full_from_module ~package ~module_name:env.file.module_name with
      | None -> ([], [])
      | Some full ->
        maybe_log
          ("Finding references for (global) " ^ Uri.to_string env.file.uri
         ^ " and stamp " ^ string_of_int stamp ^ " and tip " ^ Tip.to_string tip
          );
        family_references ~state ~full stamp tip))

let for_local_stamp ~state ~full stamp tip =
  let locs, keys = family_references ~state ~full stamp tip in
  locs @ external_references ~package:full.package keys

let all_references_for_loc_item ~state ~full:({file; package} as full) loc_item
    =
  match loc_item.loc_type with
  | TopLevelModule module_name ->
    let other_modules_references =
      package.project_files |> File_set.elements
      |> List.filter_map (fun module_name ->
          Cmt.full_from_module ~package ~module_name)
      |> List.map (fun full ->
          match Hashtbl.find_opt full.extra.file_references module_name with
          | None -> []
          | Some locs ->
            locs |> Location_set.elements
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
  | TypeDefinition (_, _, stamp) -> for_local_stamp ~state ~full stamp Type
  | Typed (_, _, (LocalReference (stamp, tip) | Definition (stamp, tip)))
  | LModule (LocalReference (stamp, tip) | Definition (stamp, tip)) ->
    maybe_log
      ("Finding references for " ^ Uri.to_string file.uri ^ " and stamp "
     ^ string_of_int stamp ^ " and tip " ^ Tip.to_string tip);
    for_local_stamp ~state ~full stamp tip
  | LModule (GlobalReference (module_name, path, tip))
  | Typed (_, _, GlobalReference (module_name, path, tip)) ->
    let locs, keys =
      global_family_references ~state ~package module_name path tip
    in
    locs @ external_references ~package keys

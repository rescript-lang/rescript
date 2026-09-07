type module_ = {
  name: string;
  implementation: string;
  interface: string option;
  is_dev: bool;
  feature: string option;
  mutable deps: string list;
}

exception Error of string

let source_extension path =
  match Filename.extension path with
  | ".res" -> Some false
  | ".resi" -> Some true
  | _ -> None

let module_name path =
  path |> Filename.basename |> Filename.remove_extension
  |> String.capitalize_ascii

let rec scan_dir ~root ~relative ~recurse ~is_dev ~ignored_dirs acc =
  let absolute = Filename.concat root relative in
  let entries =
    try Sys.readdir absolute |> Array.to_list |> List.sort String.compare
    with Sys_error _ -> []
  in
  List.fold_left
    (fun acc name ->
      let relative_path = Filename.concat relative name in
      let absolute_path = Filename.concat root relative_path in
      if Sys.is_directory absolute_path then
        if List.mem name ignored_dirs then acc else
        if recurse then
          scan_dir ~root ~relative:relative_path ~recurse ~is_dev ~ignored_dirs acc
        else acc
      else
        match source_extension name with
        | None -> acc
        | Some is_interface -> (relative_path, is_interface, is_dev) :: acc)
    acc entries

let discover ?(on_orphan = fun _ -> ()) (config : Config.t) ~prod ~features
    ~filter =
  let matches_filter =
    match filter with
    | None -> fun _ -> true
    | Some pattern ->
      let regex = try Str.regexp pattern with Failure _ -> raise (Error ("invalid filter regex: " ^ pattern)) in
      fun path -> try ignore (Str.search_forward regex path 0); true with Not_found -> false
  in
  let active_features = Hashtbl.create 16 in
  let rec validate_feature feature visiting =
    if List.mem feature visiting then
      raise (Error ("feature cycle involving " ^ feature));
    match List.assoc_opt feature config.features with
    | None -> ()
    | Some implied -> List.iter (fun name -> validate_feature name (feature :: visiting)) implied
  in
  List.iter (fun (name, _) -> validate_feature name []) config.features;
  let rec activate feature visiting =
    if List.mem feature visiting then
      raise (Error ("feature cycle involving " ^ feature));
    if not (Hashtbl.mem active_features feature) then (
      Hashtbl.add active_features feature ();
      match List.assoc_opt feature config.features with
      | None -> ()
      | Some implied -> List.iter (fun name -> activate name (feature :: visiting)) implied)
  in
  List.iter (fun feature -> activate feature []) (Option.value features ~default:[]);
  let all_features = features = None in
  let files =
    config.sources
    |> List.filter (fun (source : Config.source) ->
      not (prod && source.is_dev)
      && (all_features || Option.fold ~none:true ~some:(fun f -> Hashtbl.mem active_features f) source.feature))
    |> List.fold_left
         (fun acc (source : Config.source) ->
           scan_dir ~root:config.root ~relative:source.dir
             ~recurse:source.recurse ~is_dev:source.is_dev
             ~ignored_dirs:config.ignored_dirs acc)
         []
  in
  let table = Hashtbl.create (List.length files) in
  List.iter
    (fun (path, is_interface, is_dev) ->
      let name = module_name path in
      let implementation, interface, old_dev =
        match Hashtbl.find_opt table name with
        | None -> (None, None, is_dev)
        | Some values -> values
      in
      if is_interface then
        match interface with
        | Some previous ->
          raise
            (Error
               (Printf.sprintf "Duplicated interface %s: %s and %s" name
                  previous path))
        | None ->
          Hashtbl.replace table name
            (implementation, Some path, old_dev || is_dev)
      else
        match implementation with
        | Some previous ->
          raise
            (Error
               (Printf.sprintf "Duplicated module %s: %s and %s" name previous
                  path))
        | None ->
          Hashtbl.replace table name (Some path, interface, old_dev || is_dev))
    (List.filter (fun (path, _, _) -> matches_filter path) files);
  Hashtbl.to_seq table
  |> Seq.filter_map (fun (_, (implementation, interface, _)) ->
       match implementation, interface with
       | None, Some interface -> Some interface
       | _ -> None)
  |> List.of_seq |> List.sort String.compare |> List.iter on_orphan;
  Hashtbl.to_seq table
  |> Seq.filter_map (fun (name, (implementation, interface, is_dev)) ->
      match implementation with
      | None -> None
      | Some implementation ->
        Some {name; implementation; interface; is_dev; feature = None; deps = []})
  |> List.of_seq
  |> List.sort (fun a b -> String.compare a.name b.name)

let ast_path path =
  Filename.remove_extension path
  ^ if Filename.extension path = ".resi" then ".iast" else ".ast"

let compiler_basename config module_name =
  match config.Config.namespace, config.namespace_entry with
  | Some _, Some entry when entry = module_name -> module_name
  | Some namespace, Some _ -> module_name ^ "-@" ^ namespace
  | Some namespace, _ -> module_name ^ "-" ^ namespace
  | None, _ -> module_name

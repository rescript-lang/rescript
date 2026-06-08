open Shared_types

(* Creates the `pathsForModule` hashtbl, which maps a `moduleName` to it's `paths` (the ml/re, mli/rei, cmt, and cmti files) *)
let make_paths_for_module ~project_files_and_paths ~dependencies_files_and_paths
    =
  let paths_for_module = Hashtbl.create 30 in
  dependencies_files_and_paths
  |> List.iter (fun (mod_name, paths) ->
         Hashtbl.replace paths_for_module mod_name paths);
  project_files_and_paths
  |> List.iter (fun (mod_name, paths) ->
         Hashtbl.replace paths_for_module mod_name paths);
  paths_for_module

let override_rescript_version = ref None

let get_rescript_version () =
  match !override_rescript_version with
  | Some override_rescript_version -> override_rescript_version
  | None -> (
    (* TODO: Include patch stuff when needed *)
    let default_version = (11, 0) in
    try
      let value = Sys.getenv "RESCRIPT_VERSION" in
      let version =
        match value |> String.split_on_char '.' with
        | major :: minor :: _rest -> (
          match (int_of_string_opt major, int_of_string_opt minor) with
          | Some major, Some minor -> (major, minor)
          | _ -> default_version)
        | _ -> default_version
      in
      version
    with Not_found -> default_version)

let new_bs_package ~root_path =
  let rescript_json = Filename.concat root_path "rescript.json" in

  let parse_raw raw =
    let lib_bs =
      match !Cfg.is_doc_gen_from_compiler with
      | true -> Build_system.get_stdlib root_path
      | false -> Build_system.get_lib_bs root_path
    in
    match Yojson_helpers.from_string_opt raw with
    | Some config -> (
      let namespace = Find_files.get_namespace config in
      let rescript_version = get_rescript_version () in
      let suffix =
        match config |> Yojson_helpers.get "suffix" with
        | Some (`String suffix) -> suffix
        | _ -> ".js"
      in
      let generic_jsx_module =
        let jsx_config = config |> Yojson_helpers.get "jsx" in
        match jsx_config with
        | Some jsx_config -> (
          match jsx_config |> Yojson_helpers.get "module" with
          | Some (`String m) when String.lowercase_ascii m <> "react" -> Some m
          | _ -> None)
        | None -> None
      in
      let autocomplete =
        match config |> Yojson_helpers.get "editor" with
        | Some editor_config -> (
          match editor_config |> Yojson_helpers.get "autocomplete" with
          | Some (`Assoc map) ->
            map
            |> List.fold_left
                 (fun acc (key, value) ->
                   match value with
                   | `List items ->
                     let values =
                       items
                       |> List.filter_map (function
                            | `String s -> Some s
                            | _ -> None)
                     in
                     Misc.String_map.add key values acc
                   | _ -> acc)
                 Misc.String_map.empty
          | _ -> Misc.String_map.empty)
        | None -> Misc.String_map.empty
      in
      match lib_bs with
      | None -> None
      | Some lib_bs ->
        let cached = Cache.read_cache (Cache.target_file_from_lib_bs lib_bs) in
        let project_files, dependencies_files, paths_for_module =
          match cached with
          | Some cached ->
            ( cached.project_files,
              cached.dependencies_files,
              cached.paths_for_module )
          | None ->
            let dependencies_files_and_paths =
              match Find_files.find_dependency_files root_path config with
              | None -> []
              | Some (_dependencyDirectories, dependencies_files_and_paths) ->
                dependencies_files_and_paths
            in
            let source_directories =
              Find_files.get_source_directories ~include_dev:true
                ~base_dir:root_path config
            in
            let project_files_and_paths =
              Find_files.find_project_files
                ~public:(Find_files.get_public config)
                ~namespace ~path:root_path ~source_directories ~lib_bs
            in
            let paths_for_module =
              make_paths_for_module ~project_files_and_paths
                ~dependencies_files_and_paths
            in
            let project_files =
              project_files_and_paths |> List.map fst |> File_set.of_list
            in
            let dependencies_files =
              dependencies_files_and_paths |> List.map fst |> File_set.of_list
            in
            (project_files, dependencies_files, paths_for_module)
        in
        Some
          (let opens_from_namespace =
             match namespace with
             | None -> []
             | Some namespace ->
               let cmt = Filename.concat lib_bs namespace ^ ".cmt" in
               Hashtbl.replace paths_for_module namespace (Namespace {cmt});
               let path = [Find_files.name_space_to_name namespace] in
               [path]
           in
           let bind f x = Option.bind x f in
           let compiler_flags =
             match
               ( Yojson_helpers.get "compiler-flags" config
                 |> bind Yojson_helpers.to_list_opt,
                 Yojson_helpers.get "bsc-flags" config
                 |> bind Yojson_helpers.to_list_opt )
             with
             | Some compiler_flags, None | _, Some compiler_flags ->
               compiler_flags
             | None, None -> []
           in
           let no_pervasives =
             compiler_flags
             |> List.exists (fun s ->
                    match s with
                    | `String s -> s = "-nopervasives"
                    | _ -> false)
           in
           let opens_from_compiler_flags =
             List.fold_left
               (fun opens item ->
                 match item |> Yojson_helpers.string_opt with
                 | None -> opens
                 | Some s -> (
                   let parts = String.split_on_char ' ' s in
                   match parts with
                   | "-open" :: name :: _ ->
                     let path = name |> String.split_on_char '.' in
                     path :: opens
                   | _ -> opens))
               [] compiler_flags
           in
           let opens_from_pervasives =
             if no_pervasives then []
             else [["Stdlib"]; ["Pervasives"; "JsxModules"]]
           in
           let opens =
             opens_from_pervasives @ opens_from_namespace
             |> List.rev_append opens_from_compiler_flags
             |> List.map (fun path -> path @ ["place holder"])
           in
           {
             generic_jsx_module;
             suffix;
             rescript_version;
             root_path;
             project_files;
             dependencies_files;
             paths_for_module;
             opens;
             namespace;
             autocomplete;
           }))
    | None -> None
  in

  match Files.read_file rescript_json with
  | Some raw -> parse_raw raw
  | None ->
    Log.log ("Unable to read " ^ rescript_json);
    None

let find_root ~uri packages_by_root =
  let path = Uri.to_path uri in
  let rec loop path =
    if path = "/" then None
    else if Hashtbl.mem packages_by_root path then Some (`Root path)
    else if Files.exists (Filename.concat path "rescript.json") then
      Some (`Bs path)
    else
      let parent = Filename.dirname path in
      if parent = path then (* reached root *) None else loop parent
  in
  loop (if Sys.is_directory path then path else Filename.dirname path)

let get_package ~state ~uri =
  let open Shared_types in
  if Hashtbl.mem state.root_for_uri uri then
    Some
      (Hashtbl.find state.packages_by_root
         (Hashtbl.find state.root_for_uri uri))
  else
    match find_root ~uri state.packages_by_root with
    | None ->
      Log.log "No root directory found";
      None
    | Some (`Root root_path) ->
      Hashtbl.replace state.root_for_uri uri root_path;
      Some
        (Hashtbl.find state.packages_by_root
           (Hashtbl.find state.root_for_uri uri))
    | Some (`Bs root_path) -> (
      match new_bs_package ~root_path with
      | None -> None
      | Some package ->
        Hashtbl.replace state.root_for_uri uri package.root_path;
        Hashtbl.replace state.packages_by_root package.root_path package;
        Some package)

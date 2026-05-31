let if_debug debug name fn v = if debug then Log.log (name ^ ": " ^ fn v)
let ( /+ ) = Filename.concat
let bind f x = Option.bind x f

(* Returns a list of paths, relative to the provided `base` *)
let get_source_directories ~include_dev ~base_dir config =
  let rec handle_item current item =
    match item with
    | `List contents -> List.map (handle_item current) contents |> List.concat
    | `String text -> [current /+ text]
    | `Assoc _ -> (
      let dir =
        item |> YojsonHelpers.get "dir"
        |> bind Yojson.Safe.Util.to_string_option
        |> Option.value ~default:"Must specify directory"
      in
      let typ =
        if include_dev then "lib"
        else
          item |> YojsonHelpers.get "type"
          |> bind Yojson.Safe.Util.to_string_option
          |> Option.value ~default:"lib"
      in

      if typ = "dev" then []
      else
        match item |> YojsonHelpers.get "subdirs" with
        | None | Some (`Bool false) -> [current /+ dir]
        | Some (`Bool true) ->
          Files.collect_dirs (base_dir /+ current /+ dir)
          |> List.filter (fun name -> name <> Filename.current_dir_name)
          |> List.map (Files.relpath base_dir)
        | Some item -> (current /+ dir) :: handle_item (current /+ dir) item)
    | _ -> failwith "Invalid subdirs entry"
  in
  match config |> YojsonHelpers.get "sources" with
  | None -> []
  | Some item -> handle_item "" item

let is_compiled_file name =
  Filename.check_suffix name ".cmt" || Filename.check_suffix name ".cmti"

let is_implementation name =
  Filename.check_suffix name ".re"
  || Filename.check_suffix name ".res"
  || Filename.check_suffix name ".ml"

let is_interface name =
  Filename.check_suffix name ".rei"
  || Filename.check_suffix name ".resi"
  || Filename.check_suffix name ".mli"

let is_source_file name = is_implementation name || is_interface name

let compiled_name_space name =
  String.split_on_char '-' name
  |> List.map String.capitalize_ascii
  |> String.concat ""
  (* Remove underscores??? Whyyy bucklescript, whyyyy *)
  |> String.split_on_char '_'
  |> String.concat ""

let compiled_base_name ~namespace name =
  Filename.chop_extension name
  ^
  match namespace with
  | None -> ""
  | Some n -> "-" ^ compiled_name_space n

let get_name x =
  Filename.basename x |> Filename.chop_extension |> String.capitalize_ascii

let filter_duplicates cmts =
  (* Remove .cmt's that have .cmti's *)
  let intfs = Hashtbl.create 100 in
  cmts
  |> List.iter (fun path ->
         if
           Filename.check_suffix path ".rei"
           || Filename.check_suffix path ".mli"
           || Filename.check_suffix path ".cmti"
         then Hashtbl.add intfs (get_name path) true);
  cmts
  |> List.filter (fun path ->
         not
           ((Filename.check_suffix path ".re"
            || Filename.check_suffix path ".ml"
            || Filename.check_suffix path ".cmt")
           && Hashtbl.mem intfs (get_name path)))

let name_space_to_name n =
  n
  |> Str.split (Str.regexp "[-/@]")
  |> List.map String.capitalize_ascii
  |> String.concat ""

let get_namespace config =
  let ns = config |> YojsonHelpers.get "namespace" in
  let from_string = ns |> bind Yojson.Safe.Util.to_string_option in
  let is_namespaced =
    ns
    |> bind Yojson.Safe.Util.to_bool_option
    |> Option.value ~default:(from_string |> Option.is_some)
  in
  let either x y = if x = None then y else x in
  if is_namespaced then
    let from_name =
      config |> YojsonHelpers.get "name"
      |> bind Yojson.Safe.Util.to_string_option
    in
    either from_string from_name |> Option.map name_space_to_name
  else None

module StringSet = Set.Make (String)

let get_public config =
  let public = config |> YojsonHelpers.get "public" in
  match public with
  | None -> None
  | Some public -> (
    match public |> YojsonHelpers.to_list_opt with
    | None -> None
    | Some public ->
      Some
        (public
        |> List.filter_map Yojson.Safe.Util.to_string_option
        |> StringSet.of_list))

let collect_files directory =
  let all_files = Files.read_directory directory in
  let compileds = all_files |> List.filter is_compiled_file |> filter_duplicates in
  let sources = all_files |> List.filter is_source_file |> filter_duplicates in
  compileds
  |> Utils.filter_map (fun path ->
         let mod_name = get_name path in
         let cmt = directory /+ path in
         let res_opt =
           Utils.find
             (fun name ->
               if get_name name = mod_name then Some (directory /+ name) else None)
             sources
         in
         match res_opt with
         | None -> None
         | Some res -> Some (mod_name, SharedTypes.Impl {cmt; res}))

(* Dependency resolution uses the package graph recorded by the build system in
   .sourcedirs.json when available. If a package is not listed there, analysis
   falls back to walking up node_modules from the project root. *)
let read_sourcedirs_package_roots base =
  let source_dirs_file = base /+ "lib" /+ "bs" /+ ".sourcedirs.json" in
  let read_package_entry = function
    | `List [`String name; `String path] ->
      let path = if Filename.is_relative path then base /+ path else path in
      Some (name, path)
    | _ -> None
  in
  match Files.read_file source_dirs_file with
  | None -> []
  | Some text -> (
    match YojsonHelpers.from_string_opt text with
    | None -> []
    | Some json -> (
      match
        json |> YojsonHelpers.get "pkgs" |> bind YojsonHelpers.to_list_opt
      with
      | None -> []
      | Some packages -> packages |> List.filter_map read_package_entry))

let find_package_root ~base ~sourcedirs_package_roots name =
  match List.assoc_opt name sourcedirs_package_roots with
  | Some path when Files.exists path -> Some path
  | _ -> ModuleResolution.resolve_node_module_path ~start_path:base name

(* returns a list of (absolute path to cmt(i), relative path from base to source file) *)
let find_project_files ~public ~namespace ~path ~source_directories ~lib_bs =
  let dirs =
    source_directories |> List.map (Filename.concat path) |> StringSet.of_list
  in
  let files =
    (* Use maxDepth to prevent infinite recursion where `rescript` depends on `@rescript/runtime`,
       but `@rescript/runtime` also has `rescript` as a dev dependency *)
    dirs |> StringSet.elements
    |> List.map (fun name -> Files.collect ~max_depth:2 name is_source_file)
    |> List.concat |> StringSet.of_list
  in
  dirs
  |> if_debug true "Source directories" (fun s ->
         s |> StringSet.elements |> List.map Utils.dump_path |> String.concat " ");
  files
  |> if_debug true "Source files" (fun s ->
         s |> StringSet.elements |> List.map Utils.dump_path |> String.concat " ");

  let interfaces = Hashtbl.create 100 in
  files
  |> StringSet.iter (fun path ->
         if is_interface path then Hashtbl.replace interfaces (get_name path) path);

  let normals =
    files |> StringSet.elements
    |> Utils.filter_map (fun file ->
           if is_implementation file then (
             let module_name = get_name file in
             let resi = Hashtbl.find_opt interfaces module_name in
             Hashtbl.remove interfaces module_name;
             let base = compiled_base_name ~namespace (Files.relpath path file) in
             match resi with
             | Some resi ->
               let cmti = (lib_bs /+ base) ^ ".cmti" in
               let cmt = (lib_bs /+ base) ^ ".cmt" in
               if Files.exists cmti then
                 if Files.exists cmt then
                   (* Log.log("Intf and impl " ++ cmti ++ " " ++ cmt) *)
                   Some
                     ( module_name,
                       SharedTypes.IntfAndImpl {cmti; resi; cmt; res = file} )
                 else None
               else (
                 (* Log.log("Just intf " ++ cmti) *)
                 Log.log ("Bad source file (no cmt/cmti/cmi) " ^ (lib_bs /+ base));
                 None)
             | None ->
               let cmt = (lib_bs /+ base) ^ ".cmt" in
               if Files.exists cmt then Some (module_name, Impl {cmt; res = file})
               else (
                 Log.log ("Bad source file (no cmt/cmi) " ^ (lib_bs /+ base));
                 None))
           else None)
  in
  let result =
    normals
    |> List.filter_map (fun (name, paths) ->
           let original_name = name in
           let name =
             match namespace with
             | None -> name
             | Some namespace -> name ^ "-" ^ namespace
           in
           match public with
           | Some public ->
             if public |> StringSet.mem original_name then Some (name, paths)
             else None
           | None -> Some (name, paths))
  in
  match namespace with
  | None -> result
  | Some namespace ->
    let module_name = name_space_to_name namespace in
    let cmt = (lib_bs /+ namespace) ^ ".cmt" in
    Log.log ("adding namespace " ^ namespace ^ " : " ^ module_name ^ " : " ^ cmt);
    (module_name, Namespace {cmt}) :: result

let find_dependency_files base config =
  let deps =
    match
      ( config
        |> YojsonHelpers.get "dependencies"
        |> bind YojsonHelpers.to_list_opt,
        config
        |> YojsonHelpers.get "bs-dependencies"
        |> bind YojsonHelpers.to_list_opt )
    with
    | None, None -> []
    | Some deps, None | _, Some deps ->
      deps |> List.filter_map Yojson.Safe.Util.to_string_option
  in
  let dev_deps =
    match
      ( config
        |> YojsonHelpers.get "dev-dependencies"
        |> bind YojsonHelpers.to_list_opt,
        config
        |> YojsonHelpers.get "bs-dev-dependencies"
        |> bind YojsonHelpers.to_list_opt )
    with
    | None, None -> []
    | Some dev_deps, None | _, Some dev_deps ->
      dev_deps |> List.filter_map (fun x -> Some (Yojson.Safe.Util.to_string x))
  in
  let deps = deps @ dev_deps in
  Log.log ("Dependencies: " ^ String.concat " " deps);
  let sourcedirs_package_roots = read_sourcedirs_package_roots base in
  let dep_files =
    deps
    |> List.map (fun name ->
           let result =
             bind
               (fun path ->
                 let rescript_json_path = path /+ "rescript.json" in

                 let parse_text text =
                   match YojsonHelpers.from_string_opt text with
                   | Some inner -> (
                     let namespace = get_namespace inner in
                     let source_directories =
                       get_source_directories ~include_dev:false ~base_dir:path
                         inner
                     in
                     match BuildSystem.get_lib_bs path with
                     | None -> None
                     | Some lib_bs ->
                       let compiled_directories =
                         source_directories |> List.map (Filename.concat lib_bs)
                       in
                       let compiled_directories =
                         match namespace with
                         | None -> compiled_directories
                         | Some _ -> lib_bs :: compiled_directories
                       in
                       let project_files =
                         find_project_files ~public:(get_public inner) ~namespace
                           ~path ~source_directories ~lib_bs
                       in
                       Some (compiled_directories, project_files))
                   | None -> None
                 in

                 match Files.read_file rescript_json_path with
                 | Some text -> parse_text text
                 | None -> None)
               (find_package_root ~base ~sourcedirs_package_roots name)
           in

           match result with
           | Some (files, directories) -> (files, directories)
           | None ->
             Log.log ("Skipping nonexistent dependency: " ^ name);
             ([], []))
  in
  match BuildSystem.get_stdlib base with
  | None -> None
  | Some stdlib_directory ->
    let compiled_directories, project_files =
      let files, directories = List.split dep_files in
      (List.concat files, List.concat directories)
    in
    let all_files = project_files @ collect_files stdlib_directory in
    Some (compiled_directories, all_files)

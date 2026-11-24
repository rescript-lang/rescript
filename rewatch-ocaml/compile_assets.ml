type entry = {path: string; modified: float}
type ast_source = {ast_path: string; source_path: string}

type t = {
  files_by_directory: (string, string list) Hashtbl.t;
  ast_sources_by_directory: (string, ast_source list) Hashtbl.t;
  ast_dependencies: (string, string list) Hashtbl.t;
  ast_by_source: (string, entry) Hashtbl.t;
  cmi_by_module: (string, entry) Hashtbl.t;
  cmt_by_module: (string, entry) Hashtbl.t;
}

let ast_header path =
  try Some (Ast_header.read path)
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> None

let cleanup_extensions =
  [".cmi"; ".cmj"; ".cmt"; ".cmti"; ".ast"; ".iast"; ".mlmap"]

(* Compiler artifacts, and files a build killed while it published one left
   behind. *)
let is_managed_basename basename =
  List.exists (Filename.check_suffix basename) cleanup_extensions
  || String.starts_with ~prefix:File_util.atomic_write_prefix basename

let state_extension = function
  | ".ast" | ".iast" | ".cmi" | ".cmt" -> true
  | _ -> false

(* The names of the artifacts kept in memory, by directory. *)
let memory_names directories =
  let wanted = Hashtbl.create (List.length directories) in
  List.iter
    (fun directory ->
      Hashtbl.replace wanted (Build_file_store.key directory) directory)
    directories;
  let names = Hashtbl.create (List.length directories) in
  Build_file_store.memory_files ()
  |> List.iter (fun (key, _) ->
      match Hashtbl.find_opt wanted (Filename.dirname key) with
      | Some directory ->
        Hashtbl.replace names directory
          (Filename.basename key
          :: (Hashtbl.find_opt names directory |> Option.value ~default:[]))
      | None -> ());
  names

let read_directory ~memory_names directory =
  let names =
    match memory_names with
    | Some names ->
      Hashtbl.find_opt names directory
      |> Option.value ~default:[] |> List.sort String.compare
    | None -> (
      try File_util.directory_entries directory
      with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> [])
  in
  let files =
    names
    |> List.filter is_managed_basename
    |> List.map (Filename.concat directory)
  in
  let state_names =
    List.filter (fun name -> state_extension (Filename.extension name)) names
  in
  (files, state_names)

(* When the artifact at [path] last changed, if it exists: from the store when
   artifacts live in memory, else one [stat]. *)
let modification_time path =
  if No_artifacts.enabled then Build_file_store.modified path
  else
    match Unix.stat path with
    | metadata when metadata.Unix.st_kind = Unix.S_DIR -> None
    | metadata -> Some metadata.Unix.st_mtime
    | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> None

(* Runs on any domain: one [stat] per artifact, and a header read per AST. *)
let read_state_entry directory name =
  let path = Filename.concat directory name in
  modification_time path
  |> Option.map (fun modified ->
      let header =
        match Filename.extension name with
        | ".ast" | ".iast" -> ast_header path
        | _ -> None
      in
      (({path; modified}, name), header))

let module_key name =
  name |> Filename.remove_extension |> String.capitalize_ascii

let source_key = Platform.normalize_path_for_comparison

let add_module_artifact state (entry, name) =
  match Filename.extension name with
  | ".cmi" -> Hashtbl.replace state.cmi_by_module (module_key name) entry
  | ".cmt" -> Hashtbl.replace state.cmt_by_module (module_key name) entry
  | _ -> ()

let create directories =
  let state =
    {
      files_by_directory = Hashtbl.create (List.length directories);
      ast_sources_by_directory = Hashtbl.create (List.length directories);
      ast_dependencies = Hashtbl.create 64;
      ast_by_source = Hashtbl.create 64;
      cmi_by_module = Hashtbl.create 64;
      cmt_by_module = Hashtbl.create 64;
    }
  in
  let directories = List.sort_uniq String.compare directories in
  let memory_names =
    if No_artifacts.enabled then Some (memory_names directories) else None
  in
  let directories =
    List.map
      (fun directory -> (directory, read_directory ~memory_names directory))
      directories
  in
  (* A large project has thousands of artifacts, and reading them one after
     another was most of a small rebuild. *)
  let state_entries =
    directories
    |> List.concat_map (fun (directory, (_, state_names)) ->
        List.map (fun name -> (directory, name)) state_names)
    |> Parallel.map (fun (directory, name) -> read_state_entry directory name)
    |> Array.of_list
  in
  (* The entries keep the order of the names, so each directory's follow the
     previous directory's. *)
  let next_entry = ref 0 in
  directories
  |> List.iter (fun (directory, (files, state_names)) ->
      let count = List.length state_names in
      let state_entries =
        Array.sub state_entries !next_entry count
        |> Array.to_list |> List.filter_map Fun.id
      in
      next_entry := !next_entry + count;
      Hashtbl.replace state.files_by_directory directory files;
      let ast_sources =
        state_entries
        |> List.filter_map (fun ((entry, _), header) ->
            match header with
            | Some header ->
              Option.map
                (fun source -> (entry, source, header.Ast_header.dependencies))
                header.Ast_header.source
            | None -> None)
      in
      Hashtbl.replace state.ast_sources_by_directory directory
        (List.map
           (fun (entry, source, _) ->
             {ast_path = entry.path; source_path = source})
           ast_sources);
      List.iter
        (fun (entry, source, dependencies) ->
          Hashtbl.replace state.ast_dependencies entry.path dependencies;
          Hashtbl.replace state.ast_by_source (source_key source) entry)
        ast_sources;
      List.iter
        (fun (entry, _) -> add_module_artifact state entry)
        state_entries);
  state

let files state directory =
  Hashtbl.find_opt state.files_by_directory directory
  |> Option.value ~default:[]

let ast_sources state directory =
  Hashtbl.find_opt state.ast_sources_by_directory directory
  |> Option.value ~default:[]

let ast_dependencies state path =
  Hashtbl.find_opt state.ast_dependencies path |> Option.value ~default:[]

let ast state source = Hashtbl.find_opt state.ast_by_source (source_key source)

let cmi state key = Hashtbl.find_opt state.cmi_by_module key
let cmt state key = Hashtbl.find_opt state.cmt_by_module key

let replace_from_path table key path =
  match modification_time path with
  | Some modified -> Hashtbl.replace table key {path; modified}
  | None -> Hashtbl.remove table key

let refresh_cmi state ~key ~path =
  replace_from_path state.cmi_by_module key path

(* Without typed trees, an empty one in memory marks the module compiled. *)
let refresh_cmt state ~key ~path =
  if No_artifacts.enabled then Build_file_store.touch path;
  replace_from_path state.cmt_by_module key path

let refresh_ast state ~source ~path =
  replace_from_path state.ast_by_source (source_key source) path

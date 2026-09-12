type entry = {path: string; modified: float}

type t = {
  files_by_directory: (string, string list) Hashtbl.t;
  ast_sources_by_directory: (string, (string * string) list) Hashtbl.t;
  ast_by_source: (string, entry) Hashtbl.t;
  cmi_by_module: (string, entry) Hashtbl.t;
  cmt_by_module: (string, entry) Hashtbl.t;
}

let ast_source_location path =
  try (Ast_header.read path).source
  with error -> if File_util.path_is_missing path then None else raise error

let cleanup_extensions =
  [".cmi"; ".cmj"; ".cmt"; ".cmti"; ".ast"; ".iast"; ".res"; ".resi"; ".mlmap"]

let is_managed_basename basename =
  List.exists (Filename.check_suffix basename) cleanup_extensions

let state_extension = function
  | ".ast" | ".iast" | ".cmi" | ".cmt" -> true
  | _ -> false

let read_directory directory =
  let names =
    try Sys.readdir directory |> Array.to_list
    with error ->
      if File_util.path_is_missing directory then [] else raise error
  in
  let files =
    names
    |> List.filter (fun name ->
        List.mem (Filename.extension name) cleanup_extensions)
    |> List.map (Filename.concat directory)
  in
  let state_entries =
    names
    |> List.filter_map (fun name ->
        if not (state_extension (Filename.extension name)) then None
        else
          let path = Filename.concat directory name in
          try
            let metadata = Unix.stat path in
            if metadata.Unix.st_kind = Unix.S_DIR then None
            else Some ({path; modified = metadata.Unix.st_mtime}, name)
          with error ->
            if File_util.path_is_missing path then None else raise error)
  in
  (files, state_entries)

let module_key name =
  name |> Filename.remove_extension |> String.capitalize_ascii

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
      ast_by_source = Hashtbl.create 64;
      cmi_by_module = Hashtbl.create 64;
      cmt_by_module = Hashtbl.create 64;
    }
  in
  directories
  |> List.sort_uniq String.compare
  |> List.iter (fun directory ->
      let files, state_entries = read_directory directory in
      Hashtbl.replace state.files_by_directory directory files;
      let ast_sources =
        state_entries
        |> List.filter_map (fun (entry, name) ->
            match Filename.extension name with
            | ".ast" | ".iast" ->
              ast_source_location entry.path
              |> Option.map (fun source -> (entry, source))
            | _ -> None)
      in
      Hashtbl.replace state.ast_sources_by_directory directory
        (List.map (fun (entry, source) -> (entry.path, source)) ast_sources);
      List.iter
        (fun (entry, source) ->
          Hashtbl.replace state.ast_by_source source entry)
        ast_sources;
      List.iter (add_module_artifact state) state_entries);
  state

let files state directory =
  Hashtbl.find_opt state.files_by_directory directory
  |> Option.value ~default:[]

let ast_sources state directory =
  Hashtbl.find_opt state.ast_sources_by_directory directory
  |> Option.value ~default:[]

let ast state source = Hashtbl.find_opt state.ast_by_source source

let cmi state key = Hashtbl.find_opt state.cmi_by_module key
let cmt state key = Hashtbl.find_opt state.cmt_by_module key

let replace_from_path table key path =
  try
    Hashtbl.replace table key {path; modified = (Unix.stat path).Unix.st_mtime}
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
    Hashtbl.remove table key

let refresh_cmi state ~key ~path =
  replace_from_path state.cmi_by_module key path

let refresh_cmt state ~key ~path =
  replace_from_path state.cmt_by_module key path

let refresh_ast state ~source ~path =
  replace_from_path state.ast_by_source source path

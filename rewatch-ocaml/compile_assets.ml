type entry = {path: string; modified: float}

type t = {
  files_by_directory: (string, string list) Hashtbl.t;
  ast_sources_by_directory: (string, (string * string) list) Hashtbl.t;
  cmi_by_module: (string, entry) Hashtbl.t;
  cmt_by_module: (string, entry) Hashtbl.t;
}

let ast_source_location path =
  try
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        (try ignore (input_line channel) with End_of_file -> ());
        let rec find () =
          match input_line channel with
          | line ->
            let line = String.trim line in
            if line <> "" && not (Filename.is_relative line) then Some line
            else find ()
          | exception End_of_file -> None
        in
        find ())
  with Sys_error _ | Unix.Unix_error _ -> None

let read_directory directory =
  let entries =
    try Sys.readdir directory |> Array.to_list
    with Unix.Unix_error _ | Sys_error _ -> []
  in
  entries
  |> List.filter_map (fun name ->
       let path = Filename.concat directory name in
       try
         let metadata = Unix.stat path in
         if metadata.Unix.st_kind = Unix.S_DIR then None
         else Some ({path; modified = metadata.Unix.st_mtime}, name)
       with Unix.Unix_error _ | Sys_error _ -> None)

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
      cmi_by_module = Hashtbl.create 64;
      cmt_by_module = Hashtbl.create 64;
    }
  in
  directories |> List.sort_uniq String.compare
  |> List.iter (fun directory ->
       let entries = read_directory directory in
       Hashtbl.replace state.files_by_directory directory
         (List.map (fun (entry, _) -> entry.path) entries);
       Hashtbl.replace state.ast_sources_by_directory directory
         (entries
         |> List.filter_map (fun (entry, name) ->
              match Filename.extension name with
              | ".ast" | ".iast" ->
                ast_source_location entry.path
                |> Option.map (fun source -> (entry.path, source))
              | _ -> None));
       List.iter (add_module_artifact state) entries);
  state

let files state directory =
  Hashtbl.find_opt state.files_by_directory directory
  |> Option.value ~default:[]

let ast_sources state directory =
  Hashtbl.find_opt state.ast_sources_by_directory directory
  |> Option.value ~default:[]

let cmi state key = Hashtbl.find_opt state.cmi_by_module key
let cmt state key = Hashtbl.find_opt state.cmt_by_module key

let replace_from_path table key path =
  try
    Hashtbl.replace table key
      {path; modified = (Unix.stat path).Unix.st_mtime}
  with Unix.Unix_error _ | Sys_error _ -> Hashtbl.remove table key

let refresh_cmi state ~key ~path =
  replace_from_path state.cmi_by_module key path

let refresh_cmt state ~key ~path =
  replace_from_path state.cmt_by_module key path

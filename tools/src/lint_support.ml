open Analysis

module Analysis_json = Json

module Json = struct
  let read_file path =
    match Files.readFile path with
    | None -> Error ("error: unable to read " ^ path)
    | Some raw -> (
      match Analysis_json.parse raw with
      | None -> Error ("error: invalid json in " ^ path)
      | Some json -> Ok json)

  let bool_with_default ~default json key =
    Option.bind (json |> Analysis_json.get key) Analysis_json.bool
    |> Option.value ~default

  let string_array json key =
    match Option.bind (json |> Analysis_json.get key) Analysis_json.array with
    | None -> []
    | Some items -> items |> List.filter_map Analysis_json.string

  let compact_field (key, value) =
    match value with
    | None -> None
    | Some value -> Some (Printf.sprintf "\"%s\":%s" key value)

  let stringify_compact_object fields =
    "{" ^ String.concat "," (fields |> List.filter_map compact_field) ^ "}"
end

module Path = struct
  let is_directory path =
    match Unix.stat path with
    | {Unix.st_kind = Unix.S_DIR} -> true
    | _ -> false
    | exception _ -> false

  let normalize_rel_path path =
    path |> Files.split Filename.dir_sep |> String.concat "/"

  let display ~base path =
    if base = "" then normalize_rel_path path
    else normalize_rel_path (Files.relpath base path)
end

module SymbolKind = struct
  type t = Auto | Module | Value | Type

  let to_string = function
    | Auto -> "auto"
    | Module -> "module"
    | Value -> "value"
    | Type -> "type"

  let of_string = function
    | "auto" -> Some Auto
    | "module" -> Some Module
    | "value" -> Some Value
    | "type" -> Some Type
    | _ -> None
end

module SymbolPath = struct
  type scope = {
    env: SharedTypes.QueryEnv.t;
    path: string list;
    top_level_module: string option;
  }

  let placeholder = "place holder"

  let split path =
    path |> String.split_on_char '.'
    |> List.filter (fun segment -> segment <> "")

  let strip_placeholder path =
    match List.rev path with
    | head :: rest when head = placeholder -> List.rev rest
    | _ -> path

  let resolve_in_env ~env ~package path =
    ResolvePath.resolvePath ~env ~package ~path:(path @ [placeholder])
    |> Option.map fst

  let resolve_open_env ~package open_path =
    match strip_placeholder open_path with
    | [] -> None
    | root_module :: remainder -> (
      match ProcessCmt.fileForModule root_module ~package with
      | None -> None
      | Some file -> (
        match remainder with
        | [] -> Some (SharedTypes.QueryEnv.fromFile file)
        | _ ->
          resolve_in_env
            ~env:(SharedTypes.QueryEnv.fromFile file)
            ~package remainder))

  let direct_scope ~package path =
    match path with
    | [] -> None
    | root_module :: remainder -> (
      match ProcessCmt.fileForModule root_module ~package with
      | None -> None
      | Some file ->
        Some
          {
            env = SharedTypes.QueryEnv.fromFile file;
            path = remainder;
            top_level_module = Some root_module;
          })

  let open_scopes ~package path =
    package.SharedTypes.opens
    |> List.filter_map (fun open_path ->
           resolve_open_env ~package open_path
           |> Option.map (fun env -> {env; path; top_level_module = None}))

  let scopes ~package path =
    match direct_scope ~package path with
    | Some scope -> scope :: open_scopes ~package path
    | None -> open_scopes ~package path

  let resolve_top_level_module ~package path =
    match direct_scope ~package path with
    | Some ({env; path = []; _} : scope) -> Some env.file
    | Some _ | None -> None

  let resolve_exported ~package ~tip path =
    scopes ~package path
    |> List.find_map (fun ({env; path; _} : scope) ->
           References.exportedForTip ~env ~path ~package ~tip
           |> Option.map (fun (env, _name, stamp) -> (env, stamp)))

  let resolve_module_env ~package path =
    scopes ~package path
    |> List.find_map (fun ({env; path; top_level_module} : scope) ->
           match (top_level_module, path) with
           | Some _, [] -> Some env
           | _ -> resolve_in_env ~env ~package path)
end

module Snippet = struct
  type cache = (string, string option) Hashtbl.t

  let create_cache () = Hashtbl.create 16

  let get_source source_cache path =
    match Hashtbl.find_opt source_cache path with
    | Some source -> source
    | None ->
      let source = Files.readFile path in
      Hashtbl.add source_cache path source;
      source

  let trim_trailing_newlines value =
    let rec loop last =
      if last < 0 then ""
      else if value.[last] = '\n' || value.[last] = '\r' then loop (last - 1)
      else String.sub value 0 (last + 1)
    in
    loop (String.length value - 1)

  let of_loc ~source_cache ~path ~loc ?(context_lines_before = 2)
      ?(context_lines_after = 1) ?(skip_blank_context = true)
      ?(is_warning = false) () =
    match get_source source_cache path with
    | None -> None
    | Some source ->
      Code_frame.print ~highlight_style:Underlined ~context_lines_before
        ~context_lines_after ~skip_blank_context ~is_warning ~src:source
        ~start_pos:loc.Location.loc_start ~end_pos:loc.Location.loc_end
      |> trim_trailing_newlines
      |> fun snippet -> Some ("```text\n" ^ snippet ^ "\n```")
end

module Range = struct
  type t = int * int * int * int

  let of_loc (loc : Location.t) =
    let range = Utils.cmtLocToRange loc in
    ( range.start.line,
      range.start.character,
      range.end_.line,
      range.end_.character )

  let to_compact_json ((start_line, start_char, end_line, end_char) : t) =
    Printf.sprintf "[%d,%d,%d,%d]" start_line start_char end_line end_char

  let to_text ((start_line, start_char, end_line, end_char) : t) =
    Printf.sprintf "%d:%d-%d:%d" (start_line + 1) (start_char + 1)
      (end_line + 1) (end_char + 1)
end

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

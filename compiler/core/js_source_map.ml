type source = {relative_path: string; content: string option}

type mapping = {
  generated_line: int;
  generated_column: int;
  source_index: int;
  original_line: int;
  original_column: int;
}

type t = {
  generated_file: string;
  generated_dir: string;
  source_root: string;
  sources_content: bool;
  sources: (string, int) Hashtbl.t;
  mutable source_list: source list;
  mutable mappings: mapping list;
  mutable last_generated: (int * int) option;
}

let current : t option ref = ref None

let marker_prefix = "\000RESCRIPT_SOURCE_MAP:"
let next_marker = ref 0
let marker_locs : (int, Location.t) Hashtbl.t = Hashtbl.create 128

let is_prefix ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len
  &&
  let rec loop i =
    i = prefix_len
    || (String.unsafe_get s i = String.unsafe_get prefix i && loop (i + 1))
  in
  loop 0

let comment_of_loc (loc : Location.t) =
  match !Js_config.source_map with
  | No_source_map -> None
  | Linked ->
    if loc.loc_ghost || loc.loc_start.pos_cnum < 0 then None
    else
      let id = !next_marker in
      incr next_marker;
      Hashtbl.replace marker_locs id loc;
      Some (marker_prefix ^ string_of_int id)

let with_builder builder f =
  let old = !current in
  current := builder;
  Ext_pervasives.finally () ~clean:(fun () -> current := old) f

let normalize_slashes s =
  String.map
    (function
      | '\\' -> '/'
      | c -> c)
    s

let absolute_path path =
  if path = "" then path
  else if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let split_path path =
  path |> normalize_slashes |> String.split_on_char '/'
  |> List.filter (fun part -> part <> "")

let rec drop_common xs ys =
  match (xs, ys) with
  | x :: xs, y :: ys when x = y -> drop_common xs ys
  | _ -> (xs, ys)

let repeat x n =
  let rec loop acc n = if n <= 0 then acc else loop (x :: acc) (n - 1) in
  loop [] n

let drive_root parts =
  match parts with
  | drive :: _ when String.length drive = 2 && drive.[1] = ':' ->
    Some (String.uppercase_ascii drive)
  | _ -> None

let relative_path ~from_dir ~to_file =
  let from_dir = absolute_path from_dir in
  let to_file = absolute_path to_file in
  let from_parts = split_path from_dir in
  let to_parts = split_path to_file in
  match (drive_root from_parts, drive_root to_parts) with
  (* Cross-drive Windows paths cannot be represented as a filesystem-relative path. *)
  | Some from_drive, Some to_drive when from_drive <> to_drive ->
    normalize_slashes to_file
  | Some _, None | None, Some _ -> normalize_slashes to_file
  | _ ->
    let from_rest, to_rest = drop_common from_parts to_parts in
    let parts = repeat ".." (List.length from_rest) @ to_rest in
    if parts = [] then Filename.basename to_file else String.concat "/" parts

let make ~generated_file ~source_root ~sources_content =
  {
    generated_file = Filename.basename generated_file;
    generated_dir = Filename.dirname generated_file;
    source_root;
    sources_content;
    sources = Hashtbl.create 4;
    source_list = [];
    mappings = [];
    last_generated = None;
  }

let load_content filename =
  try Some (Ext_io.load_file filename) with _ -> None

let add_source builder filename =
  let filename =
    match filename with
    | "" | "_none_" -> !Location.input_name
    | filename -> filename
  in
  let filename = absolute_path filename in
  match Hashtbl.find_opt builder.sources filename with
  | Some index -> (index, List.nth builder.source_list index)
  | None ->
    let source =
      {
        relative_path =
          relative_path ~from_dir:builder.generated_dir ~to_file:filename;
        content = load_content filename;
      }
    in
    let index = List.length builder.source_list in
    builder.source_list <- builder.source_list @ [source];
    Hashtbl.add builder.sources filename index;
    (index, source)

let utf16_units_in_utf8_slice s start stop =
  let len = String.length s in
  let stop = min stop len in
  let rec loop i count =
    if i >= stop then count
    else
      match String.unsafe_get s i with
      | '\n' -> loop (i + 1) 0
      | c ->
        let byte = Char.code c in
        if byte < 0x80 then loop (i + 1) (count + 1)
        else if byte land 0xE0 = 0xC0 && i + 1 < stop then
          loop (i + 2) (count + 1)
        else if byte land 0xF0 = 0xE0 && i + 2 < stop then
          loop (i + 3) (count + 1)
        else if byte land 0xF8 = 0xF0 && i + 3 < stop then
          loop (i + 4) (count + 2)
        else loop (i + 1) (count + 1)
  in
  loop (max 0 start) 0

let original_column source (pos : Lexing.position) =
  match source.content with
  | None -> max 0 (pos.pos_cnum - pos.pos_bol)
  | Some content -> utf16_units_in_utf8_slice content pos.pos_bol pos.pos_cnum

let add_mapping builder ~generated_line ~generated_column (loc : Location.t) =
  if loc.loc_ghost || loc.loc_start.pos_cnum < 0 then ()
  else
    match builder.last_generated with
    | Some (line, column)
      when line = generated_line && column = generated_column ->
      ()
    | _ ->
      let source_index, source = add_source builder loc.loc_start.pos_fname in
      let original_line = max 0 (loc.loc_start.pos_lnum - 1) in
      let original_column = original_column source loc.loc_start in
      builder.mappings <-
        {
          generated_line;
          generated_column;
          source_index;
          original_line;
          original_column;
        }
        :: builder.mappings;
      builder.last_generated <- Some (generated_line, generated_column)

let take_marker_loc id =
  match Hashtbl.find_opt marker_locs id with
  | None -> None
  | Some loc ->
    Hashtbl.remove marker_locs id;
    Some loc

let mark_comment fmt comment =
  if is_prefix ~prefix:marker_prefix comment then (
    let prefix_len = String.length marker_prefix in
    let id =
      int_of_string
        (String.sub comment prefix_len (String.length comment - prefix_len))
    in
    (match (!current, take_marker_loc id) with
    | Some builder, Some loc ->
      let generated_line, generated_column = Ext_pp.position fmt in
      add_mapping builder ~generated_line ~generated_column loc
    | _ -> ());
    true)
  else false

let base64_vlq_chars =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let add_vlq buf value =
  let value = if value < 0 then (-value lsl 1) + 1 else value lsl 1 in
  let rec loop value =
    let digit = value land 31 in
    let value = value lsr 5 in
    let digit = if value > 0 then digit lor 32 else digit in
    Buffer.add_char buf base64_vlq_chars.[digit];
    if value > 0 then loop value
  in
  loop value

let compare_mapping a b =
  match compare a.generated_line b.generated_line with
  | 0 -> compare a.generated_column b.generated_column
  | n -> n

let encode_mappings mappings =
  let buf = Buffer.create 256 in
  let current_line = ref 0 in
  let previous_generated_column = ref 0 in
  let previous_source = ref 0 in
  let previous_original_line = ref 0 in
  let previous_original_column = ref 0 in
  let first_segment = ref true in
  mappings |> List.sort compare_mapping
  |> List.iter (fun mapping ->
         while !current_line < mapping.generated_line do
           Buffer.add_char buf ';';
           incr current_line;
           previous_generated_column := 0;
           first_segment := true
         done;
         if not !first_segment then Buffer.add_char buf ',';
         first_segment := false;
         add_vlq buf (mapping.generated_column - !previous_generated_column);
         add_vlq buf (mapping.source_index - !previous_source);
         add_vlq buf (mapping.original_line - !previous_original_line);
         add_vlq buf (mapping.original_column - !previous_original_column);
         previous_generated_column := mapping.generated_column;
         previous_source := mapping.source_index;
         previous_original_line := mapping.original_line;
         previous_original_column := mapping.original_column);
  Buffer.contents buf

let json builder =
  let mappings = encode_mappings builder.mappings in
  let fields =
    [
      ("version", `Int 3);
      ("file", `String builder.generated_file);
      ( "sources",
        `List
          (List.map
             (fun source -> `String source.relative_path)
             builder.source_list) );
      ("names", `List []);
      ("mappings", `String mappings);
    ]
  in
  let fields =
    if builder.source_root = "" then fields
    else fields @ [("sourceRoot", `String builder.source_root)]
  in
  let fields =
    if builder.sources_content then
      fields
      @ [
          ( "sourcesContent",
            `List
              (List.map
                 (fun source ->
                   match source.content with
                   | None -> `Null
                   | Some content -> `String content)
                 builder.source_list) );
        ]
    else fields
  in
  Yojson.Safe.to_string (`Assoc fields)

let linked_comment ~map_file =
  "//# sourceMappingURL=" ^ Filename.basename map_file ^ "\n"

module Parse : sig
  type path = Relative_path of string | Full_path of string

  type kind =
    | Syntax_error
    | Warning of {number: int; configured_as_error: bool}
    | Common_error (* type error, value can't be found *)
    | Circular_dependency
    | Unknown

  type diagnostic_entry = {
    kind: kind;
    path: path;
    message: string;
    range: Lsp.Types.Range.t option;
  }

  val is_complete : string -> bool
  val parse_log_content : string -> diagnostic_entry list
end = struct
  type position = {line: int; col: int}

  type parsed_range = {start_pos: position; end_pos: position}

  type path = Relative_path of string | Full_path of string

  type kind =
    | Syntax_error
    | Warning of {number: int; configured_as_error: bool}
    | Common_error
    | Circular_dependency
    | Unknown

  type diagnostic_entry = {
    kind: kind;
    path: path;
    message: string;
    range: Lsp.Types.Range.t option;
  }

  type location_format = Path_location | File_location

  let split_lines s = s |> String.split_on_char '\n' |> Array.of_list

  let is_blank s = String.trim s = ""

  let starts_with prefix s =
    let prefix_len = String.length prefix in
    String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

  let ends_with suffix s =
    let suffix_len = String.length suffix in
    let len = String.length s in
    len >= suffix_len && String.sub s (len - suffix_len) suffix_len = suffix

  let is_rescript_source_path s =
    (not (String.contains s ' '))
    && List.exists
         (fun suffix -> ends_with suffix s)
         [".res"; ".resi"; ".re"; ".rei"]

  let filepath_of_path path =
    if Filename.is_relative path then Relative_path path else Full_path path

  let parse_path_location line =
    (* Supported formats:

       /path/file.res:3:9
         start = 3:9
         end   = 3:9

       /path/file.res:3:5-8
         start = 3:5
         end   = 3:8

       /path/file.res:1:8-2:3
         start = 1:8
         end   = 2:3

       /path/file.res
         no range
    *)
    let line = String.trim line in

    let point_re = Str.regexp "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\)$" in
    let same_line_range_re =
      Str.regexp "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)$"
    in
    let cross_line_range_re =
      Str.regexp
        "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\):\\([0-9]+\\)$"
    in
    let make_location filepath start_line start_col end_line end_col =
      Some
        ( filepath_of_path (Str.matched_group filepath line),
          Some
            {
              start_pos =
                {
                  line = Str.matched_group start_line line |> int_of_string;
                  col = Str.matched_group start_col line |> int_of_string;
                };
              end_pos =
                {
                  line = Str.matched_group end_line line |> int_of_string;
                  col = Str.matched_group end_col line |> int_of_string;
                };
            } )
    in

    try
      if Str.string_match cross_line_range_re line 0 then
        make_location 1 2 3 4 5
      else if Str.string_match same_line_range_re line 0 then
        make_location 1 2 3 2 4
      else if Str.string_match point_re line 0 then make_location 1 2 3 2 3
      else if is_rescript_source_path line then
        Some (filepath_of_path line, None)
      else None
    with Not_found | Failure _ ->
      if is_rescript_source_path line then Some (filepath_of_path line, None)
      else None

  let parse_file_location line =
    (* Supported formats:

       File "/path/file.res", line 3, characters 5-8:
         start = 3:5
         end   = 3:8

       File "/path/file.res", lines 11-13, characters 6-7:
         start = 11:6
         end   = 13:7
    *)
    let line = String.trim line in

    let single_line_re =
      Str.regexp
        "^File \"\\([^\"]+\\)\", line \\([0-9]+\\), characters \
         \\([0-9]+\\)-\\([0-9]+\\):$"
    in
    let multi_line_re =
      Str.regexp
        "^File \"\\([^\"]+\\)\", lines \\([0-9]+\\)-\\([0-9]+\\), characters \
         \\([0-9]+\\)-\\([0-9]+\\):$"
    in

    if Str.string_match single_line_re line 0 then
      let filepath = Str.matched_group 1 line in
      let line_number = Str.matched_group 2 line |> int_of_string in
      let start_col = Str.matched_group 3 line |> int_of_string in
      let end_col = Str.matched_group 4 line |> int_of_string in
      Some
        ( filepath_of_path filepath,
          Some
            {
              start_pos = {line = line_number; col = start_col};
              end_pos = {line = line_number; col = end_col};
            } )
    else if Str.string_match multi_line_re line 0 then
      let filepath = Str.matched_group 1 line in
      let start_line = Str.matched_group 2 line |> int_of_string in
      let end_line = Str.matched_group 3 line |> int_of_string in
      let start_col = Str.matched_group 4 line |> int_of_string in
      let end_col = Str.matched_group 5 line |> int_of_string in
      Some
        ( filepath_of_path filepath,
          Some
            {
              start_pos = {line = start_line; col = start_col};
              end_pos = {line = end_line; col = end_col};
            } )
    else None

  let parse_location line =
    match parse_path_location line with
    | Some location -> Some (Path_location, location)
    | None -> (
      match parse_file_location line with
      | Some location -> Some (File_location, location)
      | None -> None)

  let is_location_line line =
    match parse_location line with
    | Some _ -> true
    | None -> false

  let is_source_code_line line =
    (* Matches lines like:
         1 │ let a = 1
         2 │ let b = "hi"
         11 | ......Todos {
         46 ┆ module Input = {

       Unnumbered OCaml gutter continuation lines are handled by
       [message_from_lines] only after a numbered source line has started an
       excerpt. A diagnostic message can itself start with [|], for example
       missing pattern-match cases.
    *)
    let rescript_re = Str.regexp "^[ \t]*[0-9]+[ \t]*│" in
    let rescript_rei_re = Str.regexp "^[ \t]*[0-9]+[ \t]*┆" in
    let ocaml_re = Str.regexp "^[ \t]*[0-9]+[ \t]*|" in
    Str.string_match rescript_re line 0
    || Str.string_match rescript_rei_re line 0
    || Str.string_match ocaml_re line 0

  let is_source_continuation_line line =
    Str.string_match (Str.regexp "^[ \t]*|") line 0

  let is_source_excerpt_line line =
    is_source_code_line line || is_source_continuation_line line

  let trim_empty_edges lines =
    let rec drop_start = function
      | [] -> []
      | x :: xs when is_blank x -> drop_start xs
      | xs -> xs
    in

    let rec drop_end xs =
      match List.rev xs with
      | [] -> []
      | x :: rest when is_blank x -> drop_end (List.rev rest)
      | _ -> xs
    in

    lines |> drop_start |> drop_end

  let find_title_index lines loc_index =
    let rec loop i =
      if i < 0 then None
      else
        let line = lines.(i) in
        let trimmed = String.trim line in

        if is_blank line || starts_with "#Start" trimmed then loop (i - 1)
        else Some i
    in
    loop (loc_index - 1)

  let warning_from_line line =
    let line = String.trim line in
    let warning_number_re = Str.regexp "^Warning number[ \t]+\\([0-9]+\\)" in
    let warning_re = Str.regexp "^Warning[ \t]+\\([0-9]+\\)" in
    let configured_as_error =
      try
        ignore
          (Str.search_forward
             (Str.regexp_string "(configured as error)")
             line 0);
        true
      with Not_found -> false
    in
    if Str.string_match warning_number_re line 0 then
      Some (Str.matched_group 1 line |> int_of_string, configured_as_error)
    else if Str.string_match warning_re line 0 then
      Some (Str.matched_group 1 line |> int_of_string, configured_as_error)
    else None

  let is_diagnostic_message_start line =
    let content = String.trim line in
    String.starts_with ~prefix:"Warning " content
    || String.starts_with ~prefix:"Error" content

  let kind_from_title title =
    match title with
    | "Syntax error!" -> Syntax_error
    | "We've found a bug for you!" -> Common_error
    | other -> (
      match warning_from_line other with
      | Some (number, configured_as_error) ->
        Warning {number; configured_as_error}
      | None -> Unknown)

  let kind_from_message lines =
    let rec loop = function
      | [] -> Unknown
      | line :: rest ->
        let content = String.trim line in
        if is_blank content || is_source_excerpt_line content then loop rest
        else if String.starts_with ~prefix:"Warning " content then
          match warning_from_line content with
          | Some (number, configured_as_error) ->
            Warning {number; configured_as_error}
          | None -> Unknown
        else if String.starts_with ~prefix:"Error" content then Unknown
        else loop rest
    in
    loop lines

  let message_from_lines lines =
    let rec filter_source_lines in_source_excerpt acc = function
      | [] -> List.rev acc
      | line :: rest ->
        let trimmed = String.trim line in
        if
          is_location_line line
          || starts_with "#Done" trimmed
          || starts_with "#Start" trimmed
        then filter_source_lines false acc rest
        else if is_source_code_line line then filter_source_lines true acc rest
        else if in_source_excerpt then
          if is_diagnostic_message_start line then
            filter_source_lines false (line :: acc) rest
          else if is_blank line then
            filter_source_lines false (line :: acc) rest
          else filter_source_lines true acc rest
        else filter_source_lines false (line :: acc) rest
    in
    lines
    |> filter_source_lines false []
    |> trim_empty_edges |> List.map String.trim |> String.concat "\n"

  let unique_preserve_order items =
    let rec loop seen acc = function
      | [] -> List.rev acc
      | item :: rest ->
        if List.mem item seen then loop seen acc rest
        else loop (item :: seen) (item :: acc) rest
    in
    loop [] [] items

  let collect_matches re group line =
    let rec loop start acc =
      try
        ignore (Str.search_forward re line start);
        let matched = Str.matched_group group line in
        loop (Str.match_end ()) (matched :: acc)
      with Not_found -> List.rev acc
    in
    loop 0 []

  let source_path_from_artifact_path path =
    if ends_with ".cmj" path then
      String.sub path 0 (String.length path - 4) ^ ".res"
    else path

  let parse_dependency_cycle_paths message_lines =
    let paren_source_path_re =
      Str.regexp "(\\([^()]+\\.\\(res\\|resi\\|re\\|rei\\)\\))"
    in
    let artifact_path_re = Str.regexp "\\([^ ]+\\.cmj\\)" in
    let source_paths =
      message_lines |> List.concat_map (collect_matches paren_source_path_re 1)
    in
    match source_paths with
    | _ :: _ -> unique_preserve_order source_paths
    | [] ->
      message_lines
      |> List.concat_map (collect_matches artifact_path_re 1)
      |> List.map source_path_from_artifact_path
      |> unique_preserve_order

  let parse_dependency_cycle_entries lines len =
    let rec collect_until_done i acc =
      if i >= len then (List.rev acc, i)
      else
        let trimmed = String.trim lines.(i) in
        if starts_with "#Done" trimmed then (List.rev acc, i)
        else collect_until_done (i + 1) (lines.(i) :: acc)
    in
    let rec loop i acc =
      if i >= len then List.rev acc
      else
        let trimmed = String.trim lines.(i) in
        if starts_with "Can't continue... Found a circular dependency" trimmed
        then
          let message_lines, next_i = collect_until_done i [] in
          let message =
            message_lines |> trim_empty_edges |> List.map String.trim
            |> String.concat "\n"
          in
          let paths = parse_dependency_cycle_paths message_lines in
          let entries =
            paths
            |> List.map (fun filepath ->
                {
                  path = filepath_of_path filepath;
                  kind = Circular_dependency;
                  message;
                  range = None;
                })
          in
          loop (next_i + 1) (List.rev_append entries acc)
        else if starts_with "FAILED: dependency cycle:" trimmed then
          let message = trimmed in
          let paths = parse_dependency_cycle_paths [trimmed] in
          let entries =
            paths
            |> List.map (fun filepath ->
                {
                  path = filepath_of_path filepath;
                  kind = Circular_dependency;
                  message;
                  range = None;
                })
          in
          loop (i + 1) (List.rev_append entries acc)
        else loop (i + 1) acc
    in
    loop 0 []

  let replace_double_newline message =
    Str.global_replace (Str.regexp "\n\n") "\n" message

  let range_of_parsed_range = function
    | None -> None
    | Some range ->
      (* Compiler log locations are 1-based, while LSP locations are 0-based.
         End columns are already exclusive in compiler output, so only line
         numbers and start columns are shifted. *)
      let minus_one v = max 0 (v - 1) in
      Some
        (Lsp.Types.Range.create
           ~start:
             (Lsp.Types.Position.create
                ~line:(range.start_pos.line |> minus_one)
                ~character:(range.start_pos.col |> minus_one))
           ~end_:
             (Lsp.Types.Position.create
                ~line:(range.end_pos.line |> minus_one)
                ~character:range.end_pos.col))

  let is_complete content =
    content |> String.split_on_char '\n' |> List.rev
    |> List.find_opt (fun line -> not (is_blank line))
    |> function
    | Some line -> starts_with "#Done" (String.trim line)
    | None -> false

  let parse_log_content (content : string) =
    let lines = split_lines content in
    let len = Array.length lines in

    let diagnostics =
      let rec loop i acc =
        if i >= len then List.rev acc
        else
          match parse_location lines.(i) with
          | Some (location_format, location) ->
            let title_index =
              match location_format with
              | Path_location -> find_title_index lines i
              | File_location -> None
            in
            loop (i + 1) ((i, title_index, location_format, location) :: acc)
          | None -> loop (i + 1) acc
      in
      loop 0 []
    in

    let rec build entries =
      match entries with
      | [] -> []
      | (loc_index, title_index, location_format, (path, range)) :: rest ->
        let next_boundary =
          match rest with
          | (next_loc_index, next_title_index, next_location_format, _) :: _
            -> (
            match next_location_format with
            | File_location -> next_loc_index
            | Path_location -> (
              match next_title_index with
              | Some i -> i
              | None -> next_loc_index))
          | [] -> len
        in

        let message_start =
          match title_index with
          | Some i -> i + 1
          | None -> loc_index + 1
        in

        let raw_message_lines =
          let rec collect i acc =
            if i >= next_boundary then List.rev acc
            else collect (i + 1) (lines.(i) :: acc)
          in
          collect message_start []
        in

        let kind =
          match location_format with
          | File_location -> kind_from_message raw_message_lines
          | Path_location -> (
            match title_index with
            | Some i -> kind_from_title (String.trim lines.(i))
            | None -> kind_from_message raw_message_lines)
        in

        let message =
          raw_message_lines |> message_from_lines |> replace_double_newline
        in

        {kind; path; message; range = range_of_parsed_range range} :: build rest
    in

    let dependency_cycle_diagnostics =
      parse_dependency_cycle_entries lines len
    in

    build diagnostics @ dependency_cycle_diagnostics
end

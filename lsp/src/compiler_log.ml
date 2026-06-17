module Parse : sig
  type path = Relative_path of string | Full_path of string

  type entry =
    | Syntax_error
    | Warning of int
    | Common_error (* type error, value can't be found *)
    | Circular_dependency
    | Unknow

  type diagnostic_entry = {entry: entry; diagnostic: Lsp.Types.Diagnostic.t}

  val parse_log_content : string -> (path * diagnostic_entry) list
end = struct
  type position = {line: int; col: int}

  type range = {start_pos: position; end_pos: position}

  type path = Relative_path of string | Full_path of string

  type entry =
    | Syntax_error
    | Warning of int
    | Common_error
    | Circular_dependency
    | Unknow

  type diagnostic_entry = {entry: entry; diagnostic: Lsp.Types.Diagnostic.t}

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

  let zero_range =
    {start_pos = {line = 0; col = 0}; end_pos = {line = 0; col = 0}}

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
         start = 0:0
         end   = 0:0
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
        Some (filepath_of_path line, zero_range)
      else None
    with Not_found | Failure _ ->
      if is_rescript_source_path line then
        Some (filepath_of_path line, zero_range)
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

  let severity_from_message lines =
    let rec loop = function
      | [] -> None
      | line :: rest -> (
        let content = String.trim line in
        if is_blank content || is_source_excerpt_line content then loop rest
        else
          match content with
          | other when String.starts_with ~prefix:"Warning " other ->
            Some Lsp.Types.DiagnosticSeverity.Warning
          | other when String.starts_with ~prefix:"Error" other ->
            Some Lsp.Types.DiagnosticSeverity.Error
          | _ -> None)
    in
    loop lines

  let is_diagnostic_message_start line =
    let content = String.trim line in
    String.starts_with ~prefix:"Warning " content
    || String.starts_with ~prefix:"Error" content

  let warning_number_from_line line =
    let line = String.trim line in
    let warning_number_re = Str.regexp "^Warning number[ \t]+\\([0-9]+\\)" in
    let warning_re = Str.regexp "^Warning[ \t]+\\([0-9]+\\)" in
    if Str.string_match warning_number_re line 0 then
      Some (Str.matched_group 1 line |> int_of_string)
    else if Str.string_match warning_re line 0 then
      Some (Str.matched_group 1 line |> int_of_string)
    else None

  let kind_from_title title =
    match title with
    | "Syntax error!" -> Syntax_error
    | "We've found a bug for you!" -> Common_error
    | other -> (
      match warning_number_from_line other with
      | Some number -> Warning number
      | None -> Unknow)

  let kind_from_message lines =
    let rec loop = function
      | [] -> Unknow
      | line :: rest ->
        let content = String.trim line in
        if is_blank content || is_source_excerpt_line content then loop rest
        else if String.starts_with ~prefix:"Warning " content then
          match warning_number_from_line content with
          | Some number -> Warning number
          | None -> Unknow
        else if String.starts_with ~prefix:"Error" content then Unknow
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
                   ( filepath_of_path filepath,
                     Circular_dependency,
                     zero_range,
                     Lsp.Types.DiagnosticSeverity.Error,
                     message ))
          in
          loop (next_i + 1) (List.rev_append entries acc)
        else if starts_with "FAILED: dependency cycle:" trimmed then
          let message = trimmed in
          let paths = parse_dependency_cycle_paths [trimmed] in
          let entries =
            paths
            |> List.map (fun filepath ->
                   ( filepath_of_path filepath,
                     Circular_dependency,
                     zero_range,
                     Lsp.Types.DiagnosticSeverity.Error,
                     message ))
          in
          loop (i + 1) (List.rev_append entries acc)
        else loop (i + 1) acc
    in
    loop 0 []

  let make_diagnostic ?severity ~range ~message () =
    (* -1 because lsp line and col is 0 based *)
    let minus_one v = if v == 0 then v else v - 1 in
    Lsp.Types.Diagnostic.create ?severity ~source:"ReScript"
      ~range:
        (Lsp.Types.Range.create
           ~start:
             (Lsp.Types.Position.create
                ~line:(range.start_pos.line |> minus_one)
                ~character:(range.start_pos.col |> minus_one))
           ~end_:
             (Lsp.Types.Position.create
                ~line:(range.end_pos.line |> minus_one)
                ~character:range.end_pos.col))
      ~message:(`String message) ()

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
      | (loc_index, title_index, location_format, (filepath, range)) :: rest ->
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

        let severity =
          match location_format with
          | File_location -> severity_from_message raw_message_lines
          | Path_location -> (
            match title_index with
            | Some i -> (
              let content = String.trim lines.(i) in
              match content with
              | "Syntax error!" | "We've found a bug for you!" ->
                Some Lsp.Types.DiagnosticSeverity.Error
              | other when String.starts_with ~prefix:"Warning number" other ->
                Some Lsp.Types.DiagnosticSeverity.Warning
              | _ -> None)
            | None -> None)
        in

        let entry =
          match location_format with
          | File_location -> kind_from_message raw_message_lines
          | Path_location -> (
            match title_index with
            | Some i -> kind_from_title (String.trim lines.(i))
            | None -> kind_from_message raw_message_lines)
        in

        let message = message_from_lines raw_message_lines in

        let diagnostic = make_diagnostic ?severity ~range ~message () in
        (filepath, {entry; diagnostic}) :: build rest
    in

    let dependency_cycle_diagnostics =
      parse_dependency_cycle_entries lines len
      |> List.map (fun (filepath, entry, range, severity, message) ->
             let diagnostic = make_diagnostic ~severity ~range ~message () in
             (filepath, {entry; diagnostic}))
    in

    build diagnostics @ dependency_cycle_diagnostics
end

(* TODO: Add more tests (fatal error), gentype warning, configured as error, The implementation `does not match the interface *)
let%expect_test "parse log" =
  let print_logs logs =
    logs
    |> List.iter (fun ((path : Parse.path), (entry : Parse.diagnostic_entry)) ->
           let path =
             match path with
             | Parse.Relative_path p -> Printf.sprintf "Relative_path(%s)" p
             | Full_path p -> Printf.sprintf "Full_path(%s)" p
           in
           print_endline
             ((match entry.entry with
              | Syntax_error -> "Syntax_error"
              | Warning number -> Printf.sprintf "Warning %d" number
              | Unknow -> "Unknow"
              | Circular_dependency -> "Circular_dependency"
              | Common_error -> "Common_error")
             ^ " - " ^ path);
           Lsp.Types.Diagnostic.yojson_of_t entry.diagnostic
           |> Yojson.Safe.pretty_to_string |> print_endline;
           print_newline ())
  in
  let example_log_1 =
    {|
    #Start(1600519680823)

      Syntax error!
      /Users/chenglou/github/reason-react/src/test.res:1:8-2:3

      1 │ let a =
      2 │ let b =
      3 │

      This let-binding misses an expression


      Warning number 8
      /Users/chenglou/github/reason-react/src/test.res:3:5-8

      1 │ let a = j`😀`
      2 │ let b = `😀`
      3 │ let None = None
      4 │ let bla: int = "
      5 │   hi

      You forgot to handle a possible case here, for example:
      Some _


      We've found a bug for you!
      /Users/chenglou/github/reason-react/src/test.res:3:9

      1 │ let a = 1
      2 │ let b = "hi"
      3 │ let a = b + 1

      This has type: string
      Somewhere wanted: int

    #Done(1600519680836)
    |}
  in

  Parse.parse_log_content example_log_1 |> print_logs;
  [%expect
    {|
    Syntax_error - Full_path(/Users/chenglou/github/reason-react/src/test.res)
    {
      "message": "This let-binding misses an expression",
      "range": {
        "end": { "character": 3, "line": 1 },
        "start": { "character": 7, "line": 0 }
      },
      "severity": 1,
      "source": "ReScript"
    }

    Warning 8 - Full_path(/Users/chenglou/github/reason-react/src/test.res)
    {
      "message": "You forgot to handle a possible case here, for example:\nSome _",
      "range": {
        "end": { "character": 8, "line": 2 },
        "start": { "character": 4, "line": 2 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Common_error - Full_path(/Users/chenglou/github/reason-react/src/test.res)
    {
      "message": "This has type: string\nSomewhere wanted: int",
      "range": {
        "end": { "character": 9, "line": 2 },
        "start": { "character": 8, "line": 2 }
      },
      "severity": 1,
      "source": "ReScript"
    }
    |}];

  let example_log_2 =
    {|
      #Start(1780532423603)
      #Done(1780532423840)

    |}
  in

  Parse.parse_log_content example_log_2 |> print_logs;
  [%expect {| |}];

  (* https://github.com/rescript-lang/rescript-vscode/issues/386#issuecomment-1221093517 *)
  let example_log_3 =
    {|
      #Start(1660943070627)
      File "/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/fragmentsUsage/Fragments.res", lines 11-13, characters 6-7:
      11 | ......Todos {
         |         ...TodoItem
         |       }
      13 |   ...
      Warning 22 [preprocessor]: Field "allTodos" has been deprecated. Reason: null
      File "/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/clientUsage/PromiseChaining.res", lines 17-21, characters 6-7:
      17 | ......os: allTodos {
         |         id
         |         text
         |         completed
         |       }
      21 |   ...
      Warning 22 [preprocessor]: Field "allTodos" has been deprecated. Reason: null
      File "/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/clientUsage/ClientBasics.res", lines 18-22, characters 6-7:
      18 | ......os: allTodos {
         |         id
         |         text
         |         completed
         |       }
      22 |   ...
      Warning 22 [preprocessor]: Field "allTodos" has been deprecated. Reason: null
      File "/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/docs/Docs.res", lines 7-11, characters 6-7:
       7 | ......os: allTodos {
         |         id
         |         text
         |         completed
         |       }
      11 |   ...
      Warning 22 [preprocessor]: Field "allTodos" has been deprecated. Reason: null
      File "/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/fragmentsUsage/Query_Fragments.res", lines 6-9, characters 6-7:
      6 | ......os: allTodos {
        |         # This references the TodoItem fragment definition module above!
        |         ...TodoItem
        |       }
      9 |   ...
      Warning 22 [preprocessor]: Field "allTodos" has been deprecated. Reason: null
      File "/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/hooksUsage/Mutation.res", lines 18-22, characters 6-7:
      18 | ......os: allTodos {
         |         id
         |         completed
         |         text
         |       }
      22 |   ...
      Warning 22 [preprocessor]: Field "allTodos" has been deprecated. Reason: null
      File "/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/hooksUsage/Query_Lazy.res", lines 3-7, characters 6-7:
      3 | ......os: allTodos {
        |         id
        |         text
        |         completed
        |       }
      7 |   ...
      Warning 22 [preprocessor]: Field "allTodos" has been deprecated. Reason: null
      File "/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/hooksUsage/Query_OverlySimple.res", lines 3-7, characters 4-5:
      3 | ....os: allTodos {
        |       id
        |       text
        |       completed
        |     }
      7 |   .
      Warning 22 [preprocessor]: Field "allTodos" has been deprecated. Reason: null
      File "/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/hooksUsage/Query_SubscribeToMore.res", lines 5-9, characters 6-7:
      5 | ......os: allTodos {
        |         id
        |         completed
        |         text
        |       }
      9 |   ...
      Warning 22 [preprocessor]: Field "allTodos" has been deprecated. Reason: null
      File "/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/hooksUsage/Query_Typical.res", lines 4-8, characters 6-7:
      4 | ......os: allTodos {
        |         id
        |         text
        |         completed
        |       }
      8 |   ...
      Warning 22 [preprocessor]: Field "allTodos" has been deprecated. Reason: null
      #Done(1660943070848)
  |}
  in

  Parse.parse_log_content example_log_3 |> print_logs;
  [%expect
    {|
    Warning 22 - Full_path(/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/fragmentsUsage/Fragments.res)
    {
      "message": "Warning 22 [preprocessor]: Field \"allTodos\" has been deprecated. Reason: null",
      "range": {
        "end": { "character": 7, "line": 12 },
        "start": { "character": 5, "line": 10 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 22 - Full_path(/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/clientUsage/PromiseChaining.res)
    {
      "message": "Warning 22 [preprocessor]: Field \"allTodos\" has been deprecated. Reason: null",
      "range": {
        "end": { "character": 7, "line": 20 },
        "start": { "character": 5, "line": 16 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 22 - Full_path(/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/clientUsage/ClientBasics.res)
    {
      "message": "Warning 22 [preprocessor]: Field \"allTodos\" has been deprecated. Reason: null",
      "range": {
        "end": { "character": 7, "line": 21 },
        "start": { "character": 5, "line": 17 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 22 - Full_path(/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/docs/Docs.res)
    {
      "message": "Warning 22 [preprocessor]: Field \"allTodos\" has been deprecated. Reason: null",
      "range": {
        "end": { "character": 7, "line": 10 },
        "start": { "character": 5, "line": 6 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 22 - Full_path(/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/fragmentsUsage/Query_Fragments.res)
    {
      "message": "Warning 22 [preprocessor]: Field \"allTodos\" has been deprecated. Reason: null",
      "range": {
        "end": { "character": 7, "line": 8 },
        "start": { "character": 5, "line": 5 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 22 - Full_path(/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/hooksUsage/Mutation.res)
    {
      "message": "Warning 22 [preprocessor]: Field \"allTodos\" has been deprecated. Reason: null",
      "range": {
        "end": { "character": 7, "line": 21 },
        "start": { "character": 5, "line": 17 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 22 - Full_path(/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/hooksUsage/Query_Lazy.res)
    {
      "message": "Warning 22 [preprocessor]: Field \"allTodos\" has been deprecated. Reason: null",
      "range": {
        "end": { "character": 7, "line": 6 },
        "start": { "character": 5, "line": 2 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 22 - Full_path(/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/hooksUsage/Query_OverlySimple.res)
    {
      "message": "Warning 22 [preprocessor]: Field \"allTodos\" has been deprecated. Reason: null",
      "range": {
        "end": { "character": 5, "line": 6 },
        "start": { "character": 3, "line": 2 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 22 - Full_path(/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/hooksUsage/Query_SubscribeToMore.res)
    {
      "message": "Warning 22 [preprocessor]: Field \"allTodos\" has been deprecated. Reason: null",
      "range": {
        "end": { "character": 7, "line": 8 },
        "start": { "character": 5, "line": 4 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 22 - Full_path(/home/pedro/Desktop/rescript-apollo-client/EXAMPLES/src/hooksUsage/Query_Typical.res)
    {
      "message": "Warning 22 [preprocessor]: Field \"allTodos\" has been deprecated. Reason: null",
      "range": {
        "end": { "character": 7, "line": 7 },
        "start": { "character": 5, "line": 3 }
      },
      "severity": 2,
      "source": "ReScript"
    }
    |}];

  (* https://github.com/rescript-lang/rescript-vscode/issues/86#issuecomment-786186698 *)
  let example_log_4 =
    {|#Start(1614285167013)

      Warning number 33
      /home/misha/projects/productionmason/web/auth/src/ErrorHandlingMiddleware.res:1:1-15

      1 │ open CommonBase
      2 │
      3 │ external jsExnToExpressError: Js.Exn.t => Express.Error.t = "%identity"

      unused open CommonBase.


      Warning number 27
      /home/misha/projects/productionmason/web/auth/src/config/Config.res:10:22-27

       8 │ external stringifyAnyWithSpace: ('a, @bs.as(json`null`) _, int) => str
           ing = "stringify"
       9 │
      10 │ let validateConfig = config => {
      11 │   let googleApplicationCredentialsPath =
      12 │     NodeJs.Process.env(NodeJs.Process.process)->Js.Dict.get("GOOGLE_AP
           PLICATION_CREDENTIALS")

      unused variable config.


      Warning number 34
      /home/misha/projects/productionmason/web/auth/src/express-handler/ExpressHandler.re:48:5-26

      46 ┆ module Input = {
      47 ┆   [@decco]
      48 ┆   type t = Request.input;
      49 ┆ };
      50 ┆

      unused type t.


      Warning number 32
      /home/misha/projects/productionmason/web/auth/src/express-handler/ExpressHandler.re

      unused value t_encode.


      We've found a bug for you!
      /home/misha/projects/productionmason/web/auth/tests/Auth_Test.res:224:45

      222 ┆ ->AsyncResult.mapOk(x => {
      223 ┆   expect(x.data.users->Array.length)->toBeGreaterThan(0)
      224 ┆   expect(x.data.users->Array.getExn(0).last_name)->toBe(Some("Glenliv
            et"))
      225 ┆   Ok()
      226 ┆ })

      This has type: int
      Somewhere wanted: array<'a>

    #Done(1614285167075)|}
  in

  Parse.parse_log_content example_log_4 |> print_logs;
  [%expect
    {|
    Warning 33 - Full_path(/home/misha/projects/productionmason/web/auth/src/ErrorHandlingMiddleware.res)
    {
      "message": "unused open CommonBase.",
      "range": {
        "end": { "character": 15, "line": 0 },
        "start": { "character": 0, "line": 0 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 27 - Full_path(/home/misha/projects/productionmason/web/auth/src/config/Config.res)
    {
      "message": "unused variable config.",
      "range": {
        "end": { "character": 27, "line": 9 },
        "start": { "character": 21, "line": 9 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 34 - Full_path(/home/misha/projects/productionmason/web/auth/src/express-handler/ExpressHandler.re)
    {
      "message": "unused type t.",
      "range": {
        "end": { "character": 26, "line": 47 },
        "start": { "character": 4, "line": 47 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 32 - Full_path(/home/misha/projects/productionmason/web/auth/src/express-handler/ExpressHandler.re)
    {
      "message": "unused value t_encode.",
      "range": {
        "end": { "character": 0, "line": 0 },
        "start": { "character": 0, "line": 0 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Common_error - Full_path(/home/misha/projects/productionmason/web/auth/tests/Auth_Test.res)
    {
      "message": "This has type: int\nSomewhere wanted: array<'a>",
      "range": {
        "end": { "character": 45, "line": 223 },
        "start": { "character": 44, "line": 223 }
      },
      "severity": 1,
      "source": "ReScript"
    }
    |}];

  let example_log_5 =
    {|#Start(1780595107359)

      Can't continue... Found a circular dependency in your code:
      Demo (src/Demo.res)
       → Other (src/Other.res)
       → Demo (src/Demo.res)
      Possible solutions:
      - Extract shared code into a new module both depend on.
      #Done(1780595107364)
      |}
  in

  Parse.parse_log_content example_log_5 |> print_logs;
  [%expect
    {|
    Circular_dependency - Relative_path(src/Demo.res)
    {
      "message": "Can't continue... Found a circular dependency in your code:\nDemo (src/Demo.res)\n→ Other (src/Other.res)\n→ Demo (src/Demo.res)\nPossible solutions:\n- Extract shared code into a new module both depend on.",
      "range": {
        "end": { "character": 0, "line": 0 },
        "start": { "character": 0, "line": 0 }
      },
      "severity": 1,
      "source": "ReScript"
    }

    Circular_dependency - Relative_path(src/Other.res)
    {
      "message": "Can't continue... Found a circular dependency in your code:\nDemo (src/Demo.res)\n→ Other (src/Other.res)\n→ Demo (src/Demo.res)\nPossible solutions:\n- Extract shared code into a new module both depend on.",
      "range": {
        "end": { "character": 0, "line": 0 },
        "start": { "character": 0, "line": 0 }
      },
      "severity": 1,
      "source": "ReScript"
    }
    |}];

  let example_log_6 =
    {|#Start(1780595245481)
    FAILED: dependency cycle: src/Demo.cmj -> src/Other.cmj -> src/Demo.cmj.
    #Done(1780595245488)|}
  in

  Parse.parse_log_content example_log_6 |> print_logs;
  [%expect
    {|
    Circular_dependency - Relative_path(src/Demo.res)
    {
      "message": "FAILED: dependency cycle: src/Demo.cmj -> src/Other.cmj -> src/Demo.cmj.",
      "range": {
        "end": { "character": 0, "line": 0 },
        "start": { "character": 0, "line": 0 }
      },
      "severity": 1,
      "source": "ReScript"
    }

    Circular_dependency - Relative_path(src/Other.res)
    {
      "message": "FAILED: dependency cycle: src/Demo.cmj -> src/Other.cmj -> src/Demo.cmj.",
      "range": {
        "end": { "character": 0, "line": 0 },
        "start": { "character": 0, "line": 0 }
      },
      "severity": 1,
      "source": "ReScript"
    }
    |}];

  let example_log_7 =
    {|#Start(1780595410580)

      We've found a bug for you!
      /tmp/my-rescript-app/src/Demo.res:1:9-15

      1 │ let a = Other.a
      2 │

      The value a can't be found in Other

    FAILED: cannot make progress due to previous errors.
    #Done(1780595410597)|}
  in

  Parse.parse_log_content example_log_7 |> print_logs;
  [%expect
    {|
    Common_error - Full_path(/tmp/my-rescript-app/src/Demo.res)
    {
      "message": "The value a can't be found in Other\n\nFAILED: cannot make progress due to previous errors.",
      "range": {
        "end": { "character": 15, "line": 0 },
        "start": { "character": 8, "line": 0 }
      },
      "severity": 1,
      "source": "ReScript"
    }
    |}];

  let example_log_8 =
    {|#Start(1780603624843)
  Error in lsp-test:

    Syntax error!
    /home/pedro/Desktop/projects/lsp-test/src/ArrayUtils.res:1:41-2:0

    1 │ let empty = arr => Array.length(arr) ===
    2 │

    Did you forget to write an expression here?

  #Done(1780603624849)|}
  in
  Parse.parse_log_content example_log_8 |> print_logs;
  [%expect
    {|
    Syntax_error - Full_path(/home/pedro/Desktop/projects/lsp-test/src/ArrayUtils.res)
    {
      "message": "Did you forget to write an expression here?",
      "range": {
        "end": { "character": 0, "line": 1 },
        "start": { "character": 40, "line": 0 }
      },
      "severity": 1,
      "source": "ReScript"
    }
    |}];

  let example_log_9 =
    {|#Start(1780630901455)

        We've found a bug for you!
        /home/pedro/Desktop/projects/lsp-test/src/ArrayUtils.res:1:41-43

        1 │ let empty = arr => Array.length(arr) == "2";
        2 │

        This has type: string
        But it's being compared to something of type: int

        You can only compare things of the same type.

        You can convert string to int with Int.fromString.

      #Done(1780630901468)|}
  in

  Parse.parse_log_content example_log_9 |> print_logs;
  [%expect
    {|
    Common_error - Full_path(/home/pedro/Desktop/projects/lsp-test/src/ArrayUtils.res)
    {
      "message": "This has type: string\nBut it's being compared to something of type: int\n\nYou can only compare things of the same type.\n\nYou can convert string to int with Int.fromString.",
      "range": {
        "end": { "character": 43, "line": 0 },
        "start": { "character": 40, "line": 0 }
      },
      "severity": 1,
      "source": "ReScript"
    }
    |}];

  let example_log_10 =
    {|#Start(1781383030890)

    Warning number 8 (configured as error)
    /home/pedro/Desktop/projects/rescript-lang.org/apps/docs/app/DocsRoutes.res:6:3-9:3

     4 │
     5 │ let f = (a: t) => {
     6 │   switch a {
     7 │   | A => ()
     8 │   | B => ()
     9 │   }
    10 │ }
    11 │

    You forgot to handle a possible case here, for example:
    | C


    Warning number 37
    /home/pedro/Desktop/projects/rescript-lang.org/apps/docs/app/DocsRoutes.res:3:1-18

    1 │ open ReactRouter.Routes
    2 │
    3 │ type t = A | B | C
    4 │
    5 │ let f = (a: t) => {

    constructor A is never used to build values.
  (However, this constructor appears in patterns.)


    Warning number 37
    /home/pedro/Desktop/projects/rescript-lang.org/apps/docs/app/DocsRoutes.res:3:1-18

    1 │ open ReactRouter.Routes
    2 │
    3 │ type t = A | B | C
    4 │
    5 │ let f = (a: t) => {

    constructor B is never used to build values.
  (However, this constructor appears in patterns.)


    Warning number 37
    /home/pedro/Desktop/projects/rescript-lang.org/apps/docs/app/DocsRoutes.res:3:1-18

    1 │ open ReactRouter.Routes
    2 │
    3 │ type t = A | B | C
    4 │
    5 │ let f = (a: t) => {

    unused constructor C.


    Warning number 32
    /home/pedro/Desktop/projects/rescript-lang.org/apps/docs/app/DocsRoutes.res:5:5

    3 │ type t = A | B | C
    4 │
    5 │ let f = (a: t) => {
    6 │   switch a {
    7 │   | A => ()

    unused value f.

  #Done(1781383030936)|}
  in
  Parse.parse_log_content example_log_10 |> print_logs;
  [%expect
    {|
    Warning 8 - Full_path(/home/pedro/Desktop/projects/rescript-lang.org/apps/docs/app/DocsRoutes.res)
    {
      "message": "You forgot to handle a possible case here, for example:\n| C",
      "range": {
        "end": { "character": 3, "line": 8 },
        "start": { "character": 2, "line": 5 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 37 - Full_path(/home/pedro/Desktop/projects/rescript-lang.org/apps/docs/app/DocsRoutes.res)
    {
      "message": "constructor A is never used to build values.\n(However, this constructor appears in patterns.)",
      "range": {
        "end": { "character": 18, "line": 2 },
        "start": { "character": 0, "line": 2 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 37 - Full_path(/home/pedro/Desktop/projects/rescript-lang.org/apps/docs/app/DocsRoutes.res)
    {
      "message": "constructor B is never used to build values.\n(However, this constructor appears in patterns.)",
      "range": {
        "end": { "character": 18, "line": 2 },
        "start": { "character": 0, "line": 2 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 37 - Full_path(/home/pedro/Desktop/projects/rescript-lang.org/apps/docs/app/DocsRoutes.res)
    {
      "message": "unused constructor C.",
      "range": {
        "end": { "character": 18, "line": 2 },
        "start": { "character": 0, "line": 2 }
      },
      "severity": 2,
      "source": "ReScript"
    }

    Warning 32 - Full_path(/home/pedro/Desktop/projects/rescript-lang.org/apps/docs/app/DocsRoutes.res)
    {
      "message": "unused value f.",
      "range": {
        "end": { "character": 5, "line": 4 },
        "start": { "character": 4, "line": 4 }
      },
      "severity": 2,
      "source": "ReScript"
    }
    |}]

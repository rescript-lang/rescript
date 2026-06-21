open Lsp
open Types

module From_diagnostics : sig
  val get :
    uri:Uri.t ->
    diagnostics:Diagnostic.t list ->
    source:string ->
    CodeAction.t list
end = struct
  let diagnostic_message (diagnostic : Diagnostic.t) =
    match diagnostic.message with
    | `String message -> message
    | `MarkupContent {value} -> value

  let ansi_escape_regex = Str.regexp "\027\\[[0-9;]*m"
  let whitespace_regex = Str.regexp "[ \t\r\n]+"

  let strip_ansi str = Str.global_replace ansi_escape_regex "" str

  let empty_range position = Range.create ~start:position ~end_:position

  let replace_text range new_text = [TextEdit.create ~range ~newText:new_text]

  let spaces length = String.make (max 0 length) ' '

  let quick_fix ~uri ~diagnostic ~title ~edits =
    let edit = WorkspaceEdit.create ~changes:[(uri, edits)] () in
    Some
      (CodeAction.create ~title ~edit ~diagnostics:[diagnostic]
         ~kind:CodeActionKind.QuickFix ~isPreferred:true ())

  let insert_before_ending_char (range : Range.t) new_text =
    let position =
      Position.create ~line:range.end_.line
        ~character:(max 0 (range.end_.character - 1))
    in
    [TextEdit.create ~range:(empty_range position) ~newText:new_text]

  let wrap_range_in_text (range : Range.t) before after =
    [
      TextEdit.create ~range:(empty_range range.start) ~newText:before;
      TextEdit.create ~range:(empty_range range.end_) ~newText:after;
    ]

  let split_fields fields =
    fields |> String.trim |> Str.split (Str.regexp "[ \t\r\n]+")

  let drop_prefix ~prefix line =
    let line = String.trim line in
    if String.starts_with ~prefix line then
      let prefix_length = String.length prefix in
      Some (String.sub line prefix_length (String.length line - prefix_length))
    else None

  let before_first_dot line =
    match String.index_opt line '.' with
    | Some index -> String.sub line 0 index
    | None -> line

  let before_defined_as typ =
    match Str.search_forward (Str.regexp_string "(defined as") typ 0 with
    | index -> String.sub typ 0 index
    | exception Not_found -> typ

  let extract_typename lines =
    lines |> String.concat " " |> strip_ansi |> before_defined_as |> String.trim
    |> Str.global_replace whitespace_regex ""

  let line_starts_with ~prefix line =
    line |> strip_ansi |> String.trim |> String.starts_with ~prefix

  let did_you_mean ~uri ~diagnostic =
    let did_you_mean_prefix = "Hint: Did you mean" in
    let did_you_mean_regex = Str.regexp "Did you mean \\([A-Za-z0-9_]+\\)" in

    let suggestion_from_line line =
      match Str.search_forward did_you_mean_regex line 0 with
      | _ -> Some (Str.matched_group 1 line)
      | exception Not_found -> None
    in

    let find_did_you_mean_line message =
      message |> String.split_on_char '\n'
      |> List.find_opt (fun line ->
             String.starts_with ~prefix:did_you_mean_prefix (String.trim line))
    in

    let message = diagnostic_message diagnostic in

    match find_did_you_mean_line message with
    | None -> None
    | Some line -> (
      match suggestion_from_line line with
      | None -> None
      | Some suggestion ->
        let edit =
          WorkspaceEdit.create
            ~changes:
              [
                ( uri,
                  [TextEdit.create ~range:diagnostic.range ~newText:suggestion]
                );
              ]
            ()
        in
        Some
          (CodeAction.create
             ~title:(Printf.sprintf "Replace with '%s'" suggestion)
             ~edit ~diagnostics:[diagnostic] ~kind:CodeActionKind.QuickFix
             ~isPreferred:true ()))

  let wrap_in_some ~uri ~diagnostic =
    let pattern_prefix = "This pattern matches values of type" in
    let pattern_type_regex =
      Str.regexp "This pattern matches values of type \\(.*\\)$"
    in
    let expected_type_prefix =
      "but a pattern was expected which matches values of type"
    in
    let expected_type_regex =
      Str.regexp
        "but a pattern was expected which matches values of type\\(.*\\)$"
    in
    let option_type_regex = Str.regexp_string "option<" in
    let is_option_type typ =
      match Str.search_forward option_type_regex typ 0 with
      | _ -> true
      | exception Not_found -> false
    in
    let type_from_pattern_line line =
      match Str.search_forward pattern_type_regex line 0 with
      | _ -> Some (String.trim (Str.matched_group 1 line))
      | exception Not_found -> None
    in
    let type_from_expected_line line =
      match Str.search_forward expected_type_regex line 0 with
      | _ -> Some (String.trim (Str.matched_group 1 line))
      | exception Not_found -> None
    in
    let rec expected_type lines =
      match lines with
      | [] -> None
      | line :: next_lines ->
        if String.starts_with ~prefix:expected_type_prefix (String.trim line)
        then
          match type_from_expected_line line with
          | Some typ when typ <> "" -> Some typ
          | _ -> (
            match next_lines with
            | next_line :: _ -> Some (String.trim next_line)
            | [] -> None)
        else expected_type next_lines
    in
    let message = diagnostic_message diagnostic in
    let lines = String.split_on_char '\n' message in
    let rec loop lines =
      match lines with
      | [] -> None
      | line :: rest ->
        if String.starts_with ~prefix:pattern_prefix (String.trim line) then
          match type_from_pattern_line line with
          | Some actual_type when not (is_option_type actual_type) -> (
            match expected_type rest with
            | Some expected_type when is_option_type expected_type ->
              let edit =
                WorkspaceEdit.create
                  ~changes:
                    [(uri, wrap_range_in_text diagnostic.range "Some(" ")")]
                  ()
              in
              Some
                (CodeAction.create ~title:"Wrap in option Some" ~edit
                   ~diagnostics:[diagnostic] ~kind:CodeActionKind.QuickFix
                   ~isPreferred:true ())
            | _ -> None)
          | _ -> None
        else loop rest
    in
    loop lines

  let simple_conversion ~uri ~(diagnostic : Diagnostic.t) =
    let prefix = "You can convert " in
    let conversion_regex =
      Str.regexp
        "You can convert \\([A-Za-z0-9_]+\\) to \\([A-Za-z0-9_]+\\) with \
         \\([A-Za-z0-9_.]+\\)\\.$"
    in
    let code_action_from_line line =
      let line = line |> strip_ansi |> String.trim in
      if String.starts_with ~prefix line then
        match Str.search_forward conversion_regex line 0 with
        | _ ->
          let from_type = Str.matched_group 1 line in
          let to_type = Str.matched_group 2 line in
          let fn = Str.matched_group 3 line in
          quick_fix ~uri ~diagnostic
            ~title:
              (Printf.sprintf "Convert %s to %s with %s" from_type to_type fn)
            ~edits:(wrap_range_in_text diagnostic.range (fn ^ "(") ")")
        | exception Not_found -> None
      else None
    in
    let rec loop lines =
      match lines with
      | [] -> None
      | line :: rest -> (
        match code_action_from_line line with
        | Some _ as code_action -> code_action
        | None -> loop rest)
    in
    diagnostic |> diagnostic_message |> String.split_on_char '\n' |> loop

  let apply_uncurried ~uri ~(diagnostic : Diagnostic.t) =
    let prefix =
      "This is an uncurried ReScript function. It must be applied with a dot."
    in
    let make_code_action () =
      let position =
        Position.create ~line:diagnostic.range.end_.line
          ~character:(diagnostic.range.end_.character + 1)
      in
      quick_fix ~uri ~diagnostic ~title:"Apply uncurried function call with dot"
        ~edits:[TextEdit.create ~range:(empty_range position) ~newText:". "]
    in
    let rec loop lines =
      match lines with
      | [] -> None
      | line :: rest ->
        if line_starts_with ~prefix line then make_code_action () else loop rest
    in
    diagnostic |> diagnostic_message |> String.split_on_char '\n' |> loop

  let simple_add_missing_cases ~uri ~(diagnostic : Diagnostic.t) ~source =
    let prefix = "You forgot to handle a possible case here, for example:" in
    let make_code_action hint =
      match
        Analysis.Codemod.transform_opt ~source
          ~pos:(diagnostic.range.start.line, diagnostic.range.start.character)
          ~debug:false ~typ:Analysis.Codemod.AddMissingCases ~hint
      with
      | None -> None
      | Some new_switch_code ->
        quick_fix ~uri ~diagnostic ~title:"Insert missing cases"
          ~edits:(replace_text diagnostic.range new_switch_code)
    in
    let rec loop lines =
      match lines with
      | [] -> None
      | line :: rest ->
        if line_starts_with ~prefix line then
          let hint = rest |> String.concat "" |> String.trim in
          if hint = "" then None else make_code_action hint
        else loop rest
    in
    diagnostic |> diagnostic_message |> String.split_on_char '\n' |> loop

  let simple_type_mismatches ~uri ~(diagnostic : Diagnostic.t) =
    let this_has_type_prefix = "This has type:" in
    let somewhere_wanted_prefix = "Somewhere wanted:" in
    let default_value = function
      | "string" -> "\"-\""
      | "bool" -> "false"
      | "int" -> "-1"
      | "float" -> "-1."
      | _ -> "assert false"
    in
    let rec collect_until_somewhere_wanted acc lines =
      match lines with
      | [] -> None
      | line :: rest -> (
        match drop_prefix ~prefix:somewhere_wanted_prefix line with
        | Some wanted -> Some (List.rev acc, wanted, rest)
        | None -> collect_until_somewhere_wanted (line :: acc) rest)
    in
    let rec collect_until_blank acc lines =
      match lines with
      | [] -> List.rev acc
      | line :: rest ->
        if line |> strip_ansi |> String.trim = "" then List.rev acc
        else collect_until_blank (line :: acc) rest
    in
    let code_action_from_types ~actual_type ~wanted_type =
      if actual_type = Printf.sprintf "option<%s>" wanted_type then
        quick_fix ~uri ~diagnostic ~title:"Unwrap optional value"
          ~edits:
            (wrap_range_in_text diagnostic.range "switch "
               (Printf.sprintf " { | None => %s | Some(v) => v }"
                  (default_value wanted_type)))
      else if Printf.sprintf "option<%s>" actual_type = wanted_type then
        quick_fix ~uri ~diagnostic ~title:"Wrap value in Some"
          ~edits:(wrap_range_in_text diagnostic.range "Some(" ")")
      else None
    in
    let rec loop lines =
      match lines with
      | [] -> None
      | line :: rest -> (
        match drop_prefix ~prefix:this_has_type_prefix line with
        | None -> loop rest
        | Some actual -> (
          match collect_until_somewhere_wanted [actual] rest with
          | None -> None
          | Some (actual_lines, wanted, rest) ->
            let wanted_lines = wanted :: collect_until_blank [] rest in
            let actual_type = extract_typename actual_lines in
            let wanted_type = extract_typename wanted_lines in
            code_action_from_types ~actual_type ~wanted_type))
    in
    diagnostic |> diagnostic_message |> String.split_on_char '\n' |> loop

  let handle_undefined_record_fields ~uri ~(diagnostic : Diagnostic.t)
      ~record_field_names ~todo_value =
    match record_field_names with
    | [] -> None
    | _ ->
      let range = diagnostic.range in
      let multiline_record_definition_body =
        range.start.line <> range.end_.line
      in
      let new_text =
        if multiline_record_definition_body then
          let padding_content_record_field =
            spaces (range.end_.character + 1)
          in
          let padding_content_end_brace = spaces (range.end_.character - 1) in
          let fields =
            record_field_names
            |> List.mapi (fun index field_name ->
                   let padding =
                     if index = 0 then "  " else padding_content_record_field
                   in
                   Printf.sprintf "%s%s: %s,\n" padding field_name todo_value)
            |> String.concat ""
          in
          fields ^ padding_content_end_brace
        else
          let prefix =
            if range.end_.character - range.start.character > 2 then ", "
            else ""
          in
          let fields =
            record_field_names
            |> List.map (fun field_name ->
                   Printf.sprintf "%s: %s" field_name todo_value)
            |> String.concat ", "
          in
          prefix ^ fields
      in
      let edit =
        WorkspaceEdit.create
          ~changes:[(uri, insert_before_ending_char range new_text)]
          ()
      in
      Some
        (CodeAction.create ~title:"Add missing record fields" ~edit
           ~diagnostics:[diagnostic] ~kind:CodeActionKind.QuickFix
           ~isPreferred:true ())

  let add_undefined_record_fields_v10 ~uri ~diagnostic =
    let prefix = "Some record fields are undefined:" in
    let message = diagnostic_message diagnostic in
    let lines = String.split_on_char '\n' message in
    let rec loop lines =
      match lines with
      | [] -> None
      | line :: rest -> (
        match drop_prefix ~prefix line with
        | None -> loop rest
        | Some fields ->
          let record_field_names =
            split_fields fields @ (rest |> List.concat_map split_fields)
          in
          (* TODO: Check whether `%todo` is a better placeholder here. v10
             diagnostics historically used failwith("TODO"), while newer
             missing-field diagnostics use `%todo`. *)
          handle_undefined_record_fields ~uri ~diagnostic ~record_field_names
            ~todo_value:"failwith(\"TODO\")")
    in
    loop lines

  let add_undefined_record_fields_v11 ~uri ~diagnostic =
    let prefix = "Some required record fields are missing:" in
    let message = diagnostic_message diagnostic in
    let lines = String.split_on_char '\n' message in
    let rec collect_until_dot acc lines =
      match lines with
      | [] -> List.rev acc
      | line :: rest ->
        let fields = line |> before_first_dot |> split_fields in
        let acc = List.rev_append fields acc in
        if String.contains line '.' then List.rev acc
        else collect_until_dot acc rest
    in
    let rec loop lines =
      match lines with
      | [] -> None
      | line :: rest -> (
        match drop_prefix ~prefix line with
        | None -> loop rest
        | Some fields ->
          let record_field_names =
            split_fields (before_first_dot fields)
            @ if String.contains line '.' then [] else collect_until_dot [] rest
          in
          handle_undefined_record_fields ~uri ~diagnostic ~record_field_names
            ~todo_value:"%todo")
    in
    loop lines

  let extractor ~uri ~diagnostic ~source =
    let code_actions = ref [] in

    let append code_action =
      match code_action with
      | Some code_action -> code_actions := code_action :: !code_actions
      | None -> ()
    in

    did_you_mean ~uri ~diagnostic |> append;
    wrap_in_some ~uri ~diagnostic |> append;
    simple_conversion ~uri ~diagnostic |> append;
    apply_uncurried ~uri ~diagnostic |> append;
    simple_add_missing_cases ~uri ~diagnostic ~source |> append;
    simple_type_mismatches ~uri ~diagnostic |> append;
    add_undefined_record_fields_v10 ~uri ~diagnostic |> append;
    add_undefined_record_fields_v11 ~uri ~diagnostic |> append;

    !code_actions

  let get ~uri ~diagnostics ~source =
    diagnostics
    |> List.map (fun diagnostic -> extractor ~uri ~diagnostic ~source)
    |> List.flatten
end

module Open_compiled_file = struct
  let create ~(uri : Uri.t) ~(state : State.t) =
    let compiled_uri =
      Helpers.get_compiled_file ~uri
        ~compiler_config:(State.compiler_config state)
        ~fs:state.fs
        ~workspace_root:(State.workspace_root state)
    in
    match compiled_uri with
    | Some uri ->
      let title = "Open compiled file" in
      [
        CodeAction.create
          ~command:
            (Command.create
               ~arguments:[`String (Uri.to_string uri)]
               ~command:Execute_commands.Open_compiled.name ~title ())
          ~title ();
      ]
    | None -> []
end

module Create_interface_file = struct
  let create ~uri ~(state : State.t) =
    let should_create =
      match Document.kind uri with
      | Res ->
        not (Fs.exists ~fs:state.fs ~follow:false (Uri.to_path uri ^ "i"))
      | _ -> false
    in

    let cmi_file =
      Helpers.get_cmi_file ~uri ~fs:state.fs
        ~compiler_config:(State.compiler_config state)
        ~workspace_root:(State.workspace_root state)
    in

    match (should_create, cmi_file) with
    | true, Some cmi_file ->
      let title = "Create interface file" in
      [
        CodeAction.create
          ~command:
            (Command.create
               ~arguments:
                 [
                   `String (Uri.to_string uri);
                   `String (Uri.of_path cmi_file |> Uri.to_string);
                 ]
               ~command:Execute_commands.Create_interface.name ~title ())
          ~title ();
      ]
    | _ -> []
end

module Switch_implementation_interface_file = struct
  let create ~uri ~(state : State.t) =
    match Document.kind uri with
    | Res ->
      let target = Uri.to_path uri ^ "i" in
      if Fs.exists ~follow:false ~fs:state.fs target then
        let title = "Switch to interface file" in
        [
          CodeAction.create
            ~command:
              (Command.create
                 ~arguments:[`String (Uri.of_path target |> Uri.to_string)]
                 ~command:Execute_commands.Switch_implementation_interface.name
                 ~title ())
            ~title ();
        ]
      else []
    | Resi ->
      let target = (Uri.to_path uri |> Filename.remove_extension) ^ ".res" in
      if Fs.exists ~follow:false ~fs:state.fs target then
        let title = "Switch to implementation file" in
        [
          CodeAction.create
            ~command:
              (Command.create
                 ~arguments:[`String (Uri.of_path target |> Uri.to_string)]
                 ~command:Execute_commands.Switch_implementation_interface.name
                 ~title ())
            ~title ();
        ]
      else []
    (* TODO: Offer "create implementation file" when a .resi has no matching
       .res file, instead of only showing the switch action for existing files. *)
    | _ -> []
end

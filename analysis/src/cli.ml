let print_string json =
  Yojson.Safe.pretty_to_string ~std:true json |> print_endline
let print_null () = `Null |> print_string
let print_list l = `List l |> print_string

let completion ~state ~debug ~path ~pos ~current_file =
  let full = Cmt.load_full_cmt_from_path ~state ~path in
  let kind_file = Files.classify_source_file current_file in
  match Files.read_file current_file with
  | None | Some "" -> print_null ()
  | Some source ->
    Commands.completion ~state ~debug ~source ~kind_file ~pos ~full
    |> List.map (fun c -> Lsp.Types.CompletionItem.yojson_of_t c)
    |> print_list

let completion_resolve ~state ~path ~module_path =
  let full = Cmt.load_full_cmt_from_path ~state ~path in
  match Commands.completion_resolve ~state ~full ~module_path with
  | None -> print_null ()
  | Some (`MarkupContent {value}) -> `String value |> print_string

let inlayhint ~state ~path ~pos ~max_length ~debug =
  let full = Cmt.load_full_cmt_from_path ~state ~path in
  let kind_file = Files.classify_source_file path in
  match Files.read_file path with
  | None -> print_null ()
  | Some source -> (
    match
      Hint.inlay ~source ~kind_file ~pos ~max_length ~full ~state ~debug
    with
    | Some hints ->
      hints
      |> List.map (fun h -> Lsp.Types.InlayHint.yojson_of_t h)
      |> print_list
    | None -> print_null ())

let code_lens ~state ~path ~debug =
  let full = Cmt.load_full_cmt_from_path ~state ~path in
  let kind_file = Files.classify_source_file path in
  match Files.read_file path with
  | None -> print_null ()
  | Some source -> (
    match Hint.code_lens ~source ~kind_file ~full ~debug with
    | Some lens ->
      lens |> List.map (fun l -> Lsp.Types.CodeLens.yojson_of_t l) |> print_list
    | None -> print_null ())

let hover ~state ~path ~pos ~current_file ~debug ~supports_markdown_links =
  let full = Cmt.load_full_cmt_from_path ~state ~path in
  let kind_file = Files.classify_source_file current_file in
  match Files.read_file current_file with
  | None -> print_null ()
  | Some source -> (
    match
      Commands.hover ~source ~kind_file ~pos ~debug ~supports_markdown_links
        ~state ~full
    with
    | Some value -> Lsp.Types.Hover.yojson_of_t value |> print_string
    | None -> print_null ())

let signature_help ~state ~path ~pos ~current_file ~debug
    ~allow_for_constructor_payloads =
  let full = Cmt.load_full_cmt_from_path ~state ~path in
  let kind_file = Files.classify_source_file current_file in
  match Files.read_file current_file with
  | None -> print_null ()
  | Some source -> (
    match
      Commands.signature_help ~source ~kind_file ~pos
        ~allow_for_constructor_payloads ~full ~state ~debug
    with
    | None -> print_null ()
    | Some s -> Lsp.Types.SignatureHelp.yojson_of_t s |> print_string)

let code_action ~state ~path ~start_pos ~end_pos ~current_file ~debug =
  let kind_file = Files.classify_source_file current_file in
  match Files.read_file current_file with
  | None -> print_null ()
  | Some source ->
    Xform.extract_code_actions ~state ~path ~start_pos ~end_pos ~source
      ~kind_file
      ~full:(Cmt.load_full_cmt_from_path ~state ~path)
      ~debug
    |> List.map (fun c -> Lsp.Types.CodeAction.yojson_of_t c)
    |> print_list

let definition ~state ~path ~pos ~debug =
  let full = Cmt.load_full_cmt_from_path ~state ~path in

  match Commands.definition ~state ~full ~pos ~debug with
  | None -> print_null ()
  | Some location -> location |> Lsp.Types.Location.yojson_of_t |> print_string

let type_definition ~state ~path ~pos ~debug =
  let full = Cmt.load_full_cmt_from_path ~state ~path in
  match Commands.type_definition ~state ~full ~pos ~debug with
  | None -> print_null ()
  | Some location -> location |> Lsp.Types.Location.yojson_of_t |> print_string

let references ~state ~path ~pos ~debug =
  let full = Cmt.load_full_cmt_from_path ~state ~path in
  let all_locs = Commands.references ~state ~full ~pos ~debug in
  if all_locs = [] then print_null ()
  else
    all_locs
    |> List.map (fun l -> Lsp.Types.Location.yojson_of_t l)
    |> print_list

let rename ~state ~path ~pos ~new_name ~debug =
  let full = Cmt.load_full_cmt_from_path ~state ~path in
  match Commands.rename ~state ~full ~pos ~new_name ~debug with
  | Some {documentChanges = Some document_changes} ->
    document_changes
    |> List.map (fun c ->
           match c with
           | `RenameFile r -> Lsp.Types.RenameFile.yojson_of_t r
           | `TextDocumentEdit te -> Lsp.Types.TextDocumentEdit.yojson_of_t te
           | `DeleteFile df -> Lsp.Types.DeleteFile.yojson_of_t df
           | `CreateFile cf -> Lsp.Types.CreateFile.yojson_of_t cf)
    |> print_list
  | _ -> print_null ()

let prepare_rename ~state ~path ~pos ~debug =
  let full = Cmt.load_full_cmt_from_path ~state ~path in
  match Commands.prepare_rename ~full ~pos ~debug with
  | None -> print_null ()
  | Some {range; placeholder = None} ->
    Lsp.Types.Range.yojson_of_t range |> print_string
  | Some {range; placeholder = Some placeholder} ->
    `Assoc
      [
        ("range", Lsp.Types.Range.yojson_of_t range);
        ("placeholder", `String placeholder);
      ]
    |> print_string

let format ~path =
  match Files.read_file path with
  | None -> print_null ()
  | Some source -> (
    let kind_file = Files.classify_source_file path in
    match Commands.format ~source ~kind_file with
    | Ok text_edits -> (
      match text_edits with
      | {newText} :: _ -> print_string (`String newText)
      | _ -> print_null ())
    | Error _ -> print_null ())

let diagnostic_syntax ~path =
  match Files.read_file path with
  | None -> print_list []
  | Some source ->
    let kind_file = Files.classify_source_file path in
    Diagnostics.document_syntax ~source ~kind_file
    |> List.map Lsp.Types.Diagnostic.yojson_of_t
    |> print_list

let semantic_tokens ~path =
  match Files.read_file path with
  | None -> print_null ()
  | Some source ->
    let kind_file = Files.classify_source_file path in
    let tokens = Semantic_tokens.semantic_tokens ~source ~kind_file in
    Lsp.Types.SemanticTokens.yojson_of_t tokens |> print_string

let document_symbol ~path =
  match Files.read_file path with
  | None -> print_null ()
  | Some source ->
    let kind_file = Files.classify_source_file path in
    let symbols = Document_symbol.get_symbols ~source ~kind_file in
    print_list (symbols |> List.map Lsp.Types.DocumentSymbol.yojson_of_t)

let create_interface ~path ~cmi_file =
  let result =
    match Files.read_file path with
    | None -> ""
    | Some source -> (
      match Create_interface.command ~source ~cmi_file with
      | Ok content -> content
      | Error _ -> "")
  in
  Printf.printf "%s" result

let test ~state ~path =
  Uri.strip_path := true;
  match Files.read_file path with
  | None -> assert false
  | Some text ->
    let lines = text |> String.split_on_char '\n' in
    let process_line i line =
      let create_current_file () =
        let current_file, cout =
          Filename.open_temp_file "def" ("txt." ^ Filename.extension path)
        in
        let remove_line_comment l =
          let len = String.length l in
          let rec loop i =
            if i + 2 <= len && l.[i] = '/' && l.[i + 1] = '/' then Some (i + 2)
            else if i + 2 < len && l.[i] = ' ' then loop (i + 1)
            else None
          in
          match loop 0 with
          | None -> l
          | Some index_after_comment ->
            String.make index_after_comment ' '
            ^ String.sub l index_after_comment (len - index_after_comment)
        in
        lines
        |> List.iteri (fun j l ->
               let line_to_output =
                 if j == i - 1 then remove_line_comment l else l
               in
               Printf.fprintf cout "%s\n" line_to_output);
        close_out cout;
        current_file
      in
      if Str.string_match (Str.regexp "^ *//[ ]*\\^") line 0 then
        let matched = Str.matched_string line in
        let len = line |> String.length in
        let mlen = String.length matched in
        let rest = String.sub line mlen (len - mlen) in
        let line = i - 1 in
        let col = mlen - 1 in
        if mlen >= 3 then (
          (match String.sub rest 0 3 with
          | "db+" -> Log.verbose := true
          | "db-" -> Log.verbose := false
          | "dv+" -> Debug.debug_level := Verbose
          | "dv-" -> Debug.debug_level := Off
          | "in+" -> Cfg.in_incremental_typechecking_mode := true
          | "in-" -> Cfg.in_incremental_typechecking_mode := false
          | "ve+" -> (
            let version = String.sub rest 3 (String.length rest - 3) in
            let version = String.trim version in
            if Debug.verbose () then
              Printf.printf "Setting version: %s\n" version;
            match String.split_on_char '.' version with
            | [major_raw; minor_raw] ->
              let version =
                (int_of_string major_raw, int_of_string minor_raw)
              in
              Packages.override_rescript_version := Some version
            | _ -> ())
          | "ve-" -> Packages.override_rescript_version := None
          | "def" ->
            print_endline
              ("Definition " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            definition ~state ~path ~pos:(line, col) ~debug:true
          | "com" ->
            print_endline
              ("Complete " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let current_file = create_current_file () in
            completion ~state ~debug:true ~path ~pos:(line, col) ~current_file;
            Sys.remove current_file
          | "cre" ->
            let module_path = String.sub rest 3 (String.length rest - 3) in
            let module_path = String.trim module_path in
            print_endline ("Completion resolve: " ^ module_path);
            completion_resolve ~state ~path ~module_path
          | "dce" ->
            print_endline ("DCE " ^ path);
            Reanalyze.Run_config.run_config.suppress <- ["src"];
            Reanalyze.Run_config.run_config.unsuppress <-
              [Filename.concat "src" "dce"];
            Dce_command.command ()
          | "doc" ->
            print_endline ("DocumentSymbol " ^ path);
            document_symbol ~path
          | "hig" ->
            print_endline ("Highlight " ^ path);
            let source = Files.read_file path |> Option.get in
            let kind_file = Files.classify_source_file path in

            Semantic_tokens.command ~debug:true
              ~emitter:(Semantic_tokens.Token.create_emitter ())
              ~source ~kind_file
          | "hov" ->
            print_endline
              ("Hover " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let current_file = create_current_file () in
            hover ~state ~supports_markdown_links:true ~path ~pos:(line, col)
              ~current_file ~debug:true;
            Sys.remove current_file
          | "she" ->
            print_endline
              ("Signature help " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let current_file = create_current_file () in
            signature_help ~state ~path ~pos:(line, col) ~current_file
              ~debug:true ~allow_for_constructor_payloads:true;
            Sys.remove current_file
          | "int" ->
            print_endline ("Create Interface " ^ path);
            let cmi_file =
              let open Filename in
              let ( ++ ) = concat in
              let name = chop_extension (basename path) ^ ".cmi" in
              let dir = dirname path in
              dir ++ parent_dir_name ++ "lib" ++ "bs" ++ "src" ++ name
            in
            create_interface ~path ~cmi_file
          | "ref" ->
            print_endline
              ("References " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            references ~state ~path ~pos:(line, col) ~debug:true
          | "pre" ->
            print_endline
              ("PrepareRename " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            prepare_rename ~state ~path ~pos:(line, col) ~debug:true
          | "ren" ->
            let new_name = String.sub rest 4 (len - mlen - 4) in
            let () =
              print_endline
                ("Rename " ^ path ^ " " ^ string_of_int line ^ ":"
               ^ string_of_int col ^ " " ^ new_name)
            in
            rename ~state ~path ~pos:(line, col) ~new_name ~debug:true
          | "typ" ->
            print_endline
              ("TypeDefinition " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            type_definition ~state ~path ~pos:(line, col) ~debug:true
          | "xfm" ->
            let current_file = create_current_file () in
            (* +2 is to ensure that the character ^ points to is what's considered the end of the selection. *)
            let end_col = col + try String.index rest '^' + 2 with _ -> 0 in
            let end_pos = (line, end_col) in
            let start_pos = (line, col) in
            if start_pos = end_pos then
              print_endline
                ("Xform " ^ path ^ " " ^ string_of_int line ^ ":"
               ^ string_of_int col)
            else
              print_endline
                ("Xform " ^ path ^ " start: " ^ Pos.to_string start_pos
               ^ ", end: " ^ Pos.to_string end_pos);

            let source =
              Files.read_file current_file |> Option.value ~default:""
            in
            let full = Cmt.load_full_cmt_from_path ~state ~path in
            let kind_file = Files.classify_source_file current_file in
            let code_actions =
              Xform.extract_code_actions ~state ~path ~start_pos ~end_pos
                ~source ~kind_file ~full ~debug:true
            in
            Sys.remove current_file;
            code_actions
            |> List.iter (fun {Lsp.Types.CodeAction.title; edit} ->
                   Printf.printf "Hit: %s\n" title;
                   match edit with
                   | Some {documentChanges} ->
                     documentChanges |> Option.get
                     |> List.iter
                          (fun
                            (dc :
                              [ `CreateFile of Lsp.Types.CreateFile.t
                              | `DeleteFile of Lsp.Types.DeleteFile.t
                              | `RenameFile of Lsp.Types.RenameFile.t
                              | `TextDocumentEdit of
                                Lsp.Types.TextDocumentEdit.t ])
                          ->
                            match dc with
                            | `TextDocumentEdit tde ->
                              let filename =
                                tde.textDocument.uri |> Uri.to_path
                                |> Filename.basename
                              in
                              Printf.printf "\nTextDocumentEdit: %s\n" filename;

                              tde.edits
                              |> List.iter
                                   (fun
                                     (edit :
                                       [ `AnnotatedTextEdit of
                                         Lsp.Types.AnnotatedTextEdit.t
                                       | `TextEdit of Lsp.Types.TextEdit.t ])
                                   ->
                                     let start_char, new_text, range =
                                       match edit with
                                       | `TextEdit te ->
                                         ( te.range.start.character,
                                           te.newText,
                                           te.range )
                                       | `AnnotatedTextEdit te ->
                                         ( te.range.start.character,
                                           te.newText,
                                           te.range )
                                     in
                                     let indent = String.make start_char ' ' in
                                     Printf.printf
                                       "%s\nnewText:\n%s<--here\n%s%s\n"
                                       (Lsp.Types.Range.yojson_of_t range
                                       |> Yojson.Safe.pretty_to_string)
                                       indent indent new_text)
                            | `CreateFile cf ->
                              let filename =
                                cf.uri |> Uri.to_path |> Filename.basename
                              in
                              Printf.printf "\nCreateFile: %s\n" filename
                            | _ ->
                              failwith "not implemented text document edit test")
                   | None -> ())
          | "c-a" ->
            let hint = String.sub rest 3 (String.length rest - 3) in
            print_endline
              ("Codemod AddMissingCases" ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let source = Files.read_file path |> Option.value ~default:"" in
            Codemod.transform ~source ~pos:(line, col) ~debug:true
              ~typ:AddMissingCases ~hint
            |> print_endline
          | "dia" -> diagnostic_syntax ~path
          | "hin" ->
            (* Get all inlay Hint between line 1 and n.
               Don't get the first line = 0.
            *)
            let line_start = 1 in
            let line_end = 34 in
            print_endline
              ("Inlay Hint " ^ path ^ " " ^ string_of_int line_start ^ ":"
             ^ string_of_int line_end);
            inlayhint ~state ~path ~pos:(line_start, line_end)
              ~max_length:(Some 25) ~debug:false
          | "cle" ->
            print_endline ("Code Lens " ^ path);
            code_lens ~state ~path ~debug:false
          | "ast" ->
            print_endline
              ("Dump AST " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let current_file = create_current_file () in
            Dump_ast.dump ~pos:(line, col) ~current_file;
            Sys.remove current_file
          | "sem" -> semantic_tokens ~path
          | _ -> ());
          print_newline ())
    in
    lines |> List.iteri process_line

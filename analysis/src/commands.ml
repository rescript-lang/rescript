let completion ~state ~debug ~source ~kind_file ~pos ~full =
  match
    Completions.get_completions ~debug ~source ~kind_file ~pos ~full ~state
      ~for_hover:false
  with
  | None -> []
  | Some (completions, full, _) ->
    completions
    |> List.map (Completion_back_end.completion_to_item ~state ~full)

let completion_resolve ~state ~(full : Shared_types.full option) ~module_path =
  (* We ignore the internal module path as of now because there's currently
     no use case for it. But, if we wanted to move resolving documentation
     for regular modules and not just file modules to the completionResolve
     hook as well, it'd be easy to implement here. *)
  let module_name, _innerModulePath =
    match module_path |> String.split_on_char '.' with
    | [module_name] -> (module_name, [])
    | module_name :: rest -> (module_name, rest)
    | [] -> raise (Failure "Invalid module path.")
  in
  let docstring =
    match full with
    | None ->
      if Debug.verbose () then
        Printf.printf "[completion_resolve] Could not load cmt\n";
      None
    | Some full -> (
      match
        Process_cmt.file_for_module ~state ~package:full.package module_name
      with
      | None ->
        if Debug.verbose () then
          Printf.printf "[completion_resolve] Did not find file for module %s\n"
            module_name;
        None
      | Some file -> Some (file.structure.docstring |> String.concat "\n\n"))
  in
  match docstring with
  | None -> None
  | Some value ->
    Some
      (`MarkupContent
         (Lsp.Types.MarkupContent.create ~kind:Lsp.Types.MarkupKind.Markdown
            ~value))

let hover ~state ~source ~kind_file ~pos ~supports_markdown_links ~full ~debug =
  let result =
    match full with
    | None -> None
    | Some full -> (
      match References.get_loc_item ~full ~pos ~debug with
      | None -> (
        if debug then
          Printf.printf
            "Nothing at that position. Now trying to use completion.\n";
        match
          Hover.get_hover_via_completions ~debug ~source ~kind_file ~pos ~state
            ~for_hover:true ~supports_markdown_links ~full:(Some full)
        with
        | None -> None
        | Some hover -> Some hover)
      | Some loc_item ->
        let is_module =
          match loc_item.loc_type with
          | LModule _ | TopLevelModule _ -> true
          | TypeDefinition _ | Typed _ | Constant _ -> false
        in
        let uri_loc_opt =
          References.definition_for_loc_item ~state ~full loc_item
        in
        let skip_zero =
          match uri_loc_opt with
          | None -> false
          | Some (_, loc) ->
            let is_interface = full.file.uri |> Uri.is_interface in
            let pos_is_zero {Lexing.pos_lnum; pos_bol; pos_cnum} =
              (not is_interface) && pos_lnum = 1 && pos_cnum - pos_bol = 0
            in
            (* Skip if range is all zero, unless it's a module *)
            (not is_module) && pos_is_zero loc.loc_start
            && pos_is_zero loc.loc_end
        in
        if skip_zero then None
        else Hover.new_hover ~state ~supports_markdown_links ~full loc_item)
  in
  match result with
  | None -> None
  | Some value ->
    Some
      (Lsp.Types.Hover.create
         ~contents:
           (`MarkupContent
              (Lsp.Types.MarkupContent.create
                 ~kind:Lsp.Types.MarkupKind.Markdown ~value))
         ())

let signature_help ~state:_ ~source ~kind_file ~pos
    ~allow_for_constructor_payloads ~full ~debug =
  Signature_help.signature_help ~debug ~source ~kind_file ~pos
    ~allow_for_constructor_payloads ~full

let definition ~state ~full ~pos ~debug =
  let location_opt =
    match full with
    | None -> None
    | Some full -> (
      match References.get_loc_item ~full ~pos ~debug with
      | None -> None
      | Some loc_item -> (
        match References.definition_for_loc_item ~state ~full loc_item with
        | None -> None
        | Some (uri, loc) when not loc.loc_ghost ->
          let is_interface = full.file.uri |> Uri.is_interface in
          let pos_is_zero {Lexing.pos_lnum; pos_bol; pos_cnum} =
            (* range is zero *)
            pos_lnum = 1 && pos_cnum - pos_bol = 0
          in
          let is_module =
            match loc_item.loc_type with
            | LModule _ | TopLevelModule _ -> true
            | TypeDefinition _ | Typed _ | Constant _ -> false
          in
          let skip_loc =
            (not is_module) && (not is_interface) && pos_is_zero loc.loc_start
            && pos_is_zero loc.loc_end
          in
          if skip_loc then None
          else
            Some
              (Lsp.Types.Location.create
                 ~range:(Utils.cmt_loc_to_range loc)
                 ~uri:(Files.canonicalize_uri uri |> Uri.from_string))
        | Some _ -> None))
  in
  location_opt

let type_definition ~state ~full ~pos ~debug =
  let maybe_location =
    match full with
    | None -> None
    | Some full -> (
      match References.get_loc_item ~full ~pos ~debug with
      | None -> None
      | Some loc_item -> (
        match References.type_definition_for_loc_item ~state ~full loc_item with
        | None -> None
        | Some (uri, loc) ->
          Some
            (Lsp.Types.Location.create
               ~range:(Utils.cmt_loc_to_range loc)
               ~uri:(Files.canonicalize_uri uri |> Uri.from_string))))
  in
  maybe_location

let references ~state ~full ~pos ~debug =
  let all_locs =
    match full with
    | None -> []
    | Some full -> (
      match References.get_loc_item ~full ~pos ~debug with
      | None -> []
      | Some loc_item ->
        let all_references =
          References.all_references_for_loc_item ~state ~full loc_item
        in
        all_references
        |> List.fold_left
             (fun acc {References.uri = uri2; loc_opt} ->
               let loc =
                 match loc_opt with
                 | Some loc -> loc
                 | None -> Uri.to_top_level_loc uri2
               in

               Lsp.Types.Location.create
                 ~range:(Utils.cmt_loc_to_range loc)
                 ~uri:(Uri.to_string uri2 |> Uri.from_string)
               :: acc)
             [])
  in
  all_locs

let rename ~state ~full ~pos ~new_name ~debug =
  let result =
    match full with
    | None -> None
    | Some full -> (
      match References.get_loc_item ~full ~pos ~debug with
      | None -> None
      | Some loc_item ->
        let all_references =
          References.all_references_for_loc_item ~state ~full loc_item
        in
        let references_to_toplevel_modules =
          all_references
          |> Utils.filter_map (fun {References.uri = uri2; loc_opt} ->
                 if loc_opt = None then Some uri2 else None)
        in
        let references_to_items =
          all_references
          |> Utils.filter_map (function
               | {References.uri = uri2; loc_opt = Some loc} -> Some (uri2, loc)
               | {loc_opt = None} -> None)
        in
        let file_renames =
          references_to_toplevel_modules
          |> List.map (fun uri ->
                 let path = Uri.to_path uri in
                 let dir =
                   match Filename.dirname path with
                   | "." -> ""
                   | other -> other
                 in
                 let new_path =
                   Filename.concat dir (new_name ^ Filename.extension path)
                 in
                 `RenameFile
                   (Lsp.Types.RenameFile.create
                      ~newUri:
                        (new_path |> Uri.from_path |> Uri.to_string
                       |> Uri.from_path)
                      ~oldUri:(uri |> Uri.to_string |> Uri.from_string)
                      ()))
        in
        let text_document_edits =
          let module String_map = Misc.String_map in
          let text_edits_by_uri =
            references_to_items
            |> List.map (fun (uri, loc) -> (Uri.to_string uri, loc))
            |> List.fold_left
                 (fun acc (uri, loc) ->
                   let text_edit =
                     `TextEdit
                       (Lsp.Types.TextEdit.create ~newText:new_name
                          ~range:(Utils.cmt_loc_to_range loc))
                   in
                   match String_map.find_opt uri acc with
                   | None -> String_map.add uri [text_edit] acc
                   | Some prev_edits ->
                     String_map.add uri (text_edit :: prev_edits) acc)
                 String_map.empty
          in
          String_map.fold
            (fun uri edits acc ->
              let text_document =
                Lsp.Types.OptionalVersionedTextDocumentIdentifier.create
                  ~version:0 ~uri:(Uri.from_string uri) ()
              in
              let text_document_edit =
                `TextDocumentEdit
                  (Lsp.Types.TextDocumentEdit.create ~edits
                     ~textDocument:text_document)
              in
              text_document_edit :: acc)
            text_edits_by_uri []
        in
        let document_changes = file_renames @ text_document_edits in
        Some
          (Lsp.Types.WorkspaceEdit.create ~documentChanges:document_changes ()))
  in
  result

type prepare_rename_result = {
  range: Lsp.Types.Range.t;
  placeholder: string option;
}

let prepare_rename ~state:_ ~full ~pos ~debug =
  match full with
  | None -> None
  | Some full -> (
    match References.get_loc_item ~full ~pos ~debug with
    | None -> None
    | Some loc_item ->
      let range = Utils.cmt_loc_to_range loc_item.loc in
      let placeholder_opt =
        match loc_item.loc_type with
        | Typed (name, _, _) | TopLevelModule name | TypeDefinition (name, _, _)
          ->
          Some name
        | _ -> None
      in
      Some {range; placeholder = placeholder_opt})

let format ~state:_ ~source ~kind_file =
  let create_range text =
    let lines = text |> String.split_on_char '\n' in
    let lines_len = List.length lines in
    let character =
      match List.nth_opt lines lines_len with
      | Some line -> String.length line
      | None -> 0
    in
    let range =
      Lsp.Types.Range.create
        ~start:(Lsp.Types.Position.create ~line:0 ~character:0)
        ~end_:(Lsp.Types.Position.create ~line:(lines_len - 1) ~character)
    in
    Lsp.Types.TextEdit.create ~newText:text ~range
  in

  let result =
    match kind_file with
    | Files.Res -> (
      let {Res_driver.parsetree = structure; comments; diagnostics} =
        Res_driver.parsing_engine.parse_implementation_from_source
          ~for_printer:true ~source
      in
      match List.length diagnostics > 0 with
      | true -> Error "Document has syntax errors"
      | false ->
        Ok (Res_printer.print_implementation ~comments structure |> create_range)
      )
    | Resi -> (
      let {Res_driver.parsetree = signature; comments; diagnostics} =
        Res_driver.parsing_engine.parse_interface_from_source ~for_printer:true
          ~source
      in
      match List.length diagnostics > 0 with
      | true -> Error "Document has syntax errors"
      | false ->
        Ok (Res_printer.print_interface ~comments signature |> create_range))
    | Other -> Error "Failed to format, file not supported"
  in

  match result with
  | Ok text_edit -> Ok [text_edit]
  | Error e -> Error e

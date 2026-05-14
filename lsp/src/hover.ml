open Lsp.Types

let getCompletions ~debug ~path ~pos ~currentFile ~forHover =
  let textOpt = Some currentFile in
  match textOpt with
  | None | Some "" -> None
  | Some text -> (
    match
      Analysis.CompletionFrontEnd.completionWithParser ~debug ~path
        ~posCursor:pos ~currentFile ~text
    with
    | None -> None
    | Some (completable, scope) -> (
      (* uncomment when debugging *)
      if false then (
        Printf.printf "\nScope from frontend:\n";
        List.iter
          (fun item ->
            Printf.printf "%s\n"
              (Analysis.SharedTypes.ScopeTypes.item_to_string item))
          scope;
        print_newline ());
      (* Only perform expensive ast operations if there are completables *)
      match Analysis.Cmt.loadFullCmtFromPath ~path with
      | None -> None
      | Some full ->
        let env = Analysis.SharedTypes.QueryEnv.fromFile full.file in
        let completables =
          completable
          |> Analysis.CompletionBackEnd.processCompletable ~debug ~full ~pos
               ~scope ~env ~forHover
        in
        Some (completables, full, scope)))

(* Leverages autocomplete functionality to produce a hover for a position. This
   makes it (most often) work with unsaved content. *)
let getHoverViaCompletions ~debug ~path ~pos ~currentFile ~forHover
    ~supportsMarkdownLinks =
  match getCompletions ~debug ~path ~pos ~currentFile ~forHover with
  | None -> None
  | Some (completions, ({file; package} as full), scope) -> (
    let rawOpens = Analysis.Scope.getRawOpens scope in
    match completions with
    | {kind = Label typString; docstring} :: _ ->
      let parts =
        docstring
        @ if typString = "" then [] else [Analysis.Markdown.codeBlock typString]
      in

      Some (String.concat "\n\n" parts)
    | {kind = Field _; env; docstring} :: _ -> (
      let opens =
        Analysis.CompletionBackEnd.getOpens ~debug ~rawOpens ~package ~env
      in
      match
        Analysis.CompletionBackEnd.completionsGetTypeEnv2 ~debug ~full ~rawOpens
          ~opens ~pos completions
      with
      | Some (typ, _env) ->
        let typeString =
          Analysis.Hover.hoverWithExpandedTypes ~file ~package ~docstring
            ~supportsMarkdownLinks typ
        in
        Some typeString
      | None -> None)
    | {env} :: _ -> (
      let opens =
        Analysis.CompletionBackEnd.getOpens ~debug ~rawOpens ~package ~env
      in
      match
        Analysis.CompletionBackEnd.completionsGetTypeEnv2 ~debug ~full ~rawOpens
          ~opens ~pos completions
      with
      | Some (typ, _env) ->
        let typeString =
          Analysis.Hover.hoverWithExpandedTypes ~file ~package
            ~supportsMarkdownLinks typ
        in
        Some typeString
      | None -> None)
    | _ -> None)

let create ~(position : Position.t) ~(uri : DocumentUri.t)
    (server : State.t Server.t) =
  let path = DocumentUri.to_path uri in
  let pos = (position.line, position.character) in

  (* NOTE: Should be a config *)
  let supportsMarkdownLinks = true in

  let currentFile =
    (Document_store.get_document ~uri server.state.store).text
  in
  let debug = false in

  let result =
    match Analysis.Cmt.loadFullCmtFromPath ~path with
    | None -> None
    | Some full -> (
      match Analysis.References.getLocItem ~full ~pos ~debug with
      | None ->
        getHoverViaCompletions ~debug ~path ~pos ~currentFile ~forHover:true
          ~supportsMarkdownLinks:false
      | Some locItem ->
        let isModule =
          match locItem.locType with
          | LModule _ | TopLevelModule _ -> true
          | TypeDefinition _ | Typed _ | Constant _ -> false
        in
        let uriLocOpt =
          Analysis.References.definitionForLocItem ~full locItem
        in
        let skipZero =
          match uriLocOpt with
          | None -> false
          | Some (_, loc) ->
            let isInterface = full.file.uri |> Analysis.Uri.isInterface in
            let posIsZero {Lexing.pos_lnum; pos_bol; pos_cnum} =
              (not isInterface) && pos_lnum = 1 && pos_cnum - pos_bol = 0
            in
            (* Skip if range is all zero, unless it's a module *)
            (not isModule) && posIsZero loc.loc_start && posIsZero loc.loc_end
        in
        if skipZero then None
        else Analysis.Hover.newHover ~supportsMarkdownLinks ~full locItem)
  in

  match result with
  | None -> None
  | Some value ->
    Some
      (Hover.create
         ~contents:
           (`MarkupContent
              (MarkupContent.create ~kind:MarkupKind.Markdown ~value))
         ())

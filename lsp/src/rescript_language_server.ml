open Lsp
open Types

let version = Version.version

let initialization () =
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create ~openClose:true
         ~change:TextDocumentSyncKind.Full ~willSave:false
         ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
         ~willSaveWaitUntil:false ())
  in
  let completionProvider =
    CompletionOptions.create
      ~triggerCharacters:["."; ">"; "@"; "~"; "\""; "="; "("]
      ~resolveProvider:true ()
  in
  let codeLensProvider = CodeLensOptions.create ~resolveProvider:false () in
  let signatureHelpProvider =
    SignatureHelpOptions.create ~triggerCharacters:["("]
      ~retriggerCharacters:["="; ","] ()
  in
  let inlayHintProvider =
    `InlayHintOptions (InlayHintOptions.create ~resolveProvider:false ())
  in
  let renameProvider =
    `RenameOptions (RenameOptions.create ~prepareProvider:true ())
  in
  let workspace =
    let workspaceFolders =
      WorkspaceFoldersServerCapabilities.create ~supported:true
        ~changeNotifications:(`Bool true) ()
    in
    ServerCapabilities.create_workspace ~workspaceFolders ()
  in
  let semanticTokensProvider =
    let legend =
      SemanticTokensLegend.create ~tokenModifiers:[]
        ~tokenTypes:
          [
            "operator";
            "variable";
            "type";
            (* emit jsx-tag < and > in <div> as modifier *)
            "modifier";
            "namespace";
            "enumMember";
            "property";
            (* emit jsxlowercase, div in <div> as interface *)
            "interface";
          ]
    in
    let full = `Full (SemanticTokensOptions.create_full ~delta:false ()) in
    `SemanticTokensOptions (SemanticTokensOptions.create ~legend ~full ())
  in
  let codeActionProvider =
    `CodeActionOptions (CodeActionOptions.create ~resolveProvider:false ())
  in
  let executeCommandProvider =
    ExecuteCommandOptions.create
      ~commands:
        [
          Execute_commands.Open_compiled.name;
          Execute_commands.Switch_implementation_interface.name;
          Execute_commands.Dump_server_state.name;
          Execute_commands.Dump_cmt.name;
        ]
      ()
  in
  (* TODO: Revisit interFileDependencies when pull diagnostics can report
     dependency-aware invalidation. For now compiler-log diagnostics are
     refreshed as full snapshots, so advertising false is more accurate. *)
  let diagnosticProvider =
    `DiagnosticOptions
      (DiagnosticOptions.create ~workspaceDiagnostics:false
         ~interFileDependencies:false ())
  in
  let capabilities =
    ServerCapabilities.create ~textDocumentSync ~completionProvider
      ~codeLensProvider ~hoverProvider:(`Bool true) ~signatureHelpProvider
      ~renameProvider ~workspace ~semanticTokensProvider ~inlayHintProvider
      ~definitionProvider:(`Bool true) ~typeDefinitionProvider:(`Bool true)
      ~codeActionProvider ~documentSymbolProvider:(`Bool true)
      ~referencesProvider:(`Bool true) ~documentFormattingProvider:(`Bool true)
      ~executeCommandProvider ~diagnosticProvider ()
  in
  let serverInfo =
    InitializeResult.create_serverInfo ~name:"rescript-language-server" ~version
      ()
  in
  InitializeResult.create ~capabilities ~serverInfo ()

let get_updated_diagnostics_from_log (state : State.t) diagnostics =
  let workspace_root = State.workspace_root state in
  let compiler_log =
    Diagnostics.collect_diagnostics_from_log_using_source_dirs workspace_root
      state.fs
  in
  Diagnostics.update_from_compiler_log ~workspace_root ~doc_store:state.store
    compiler_log diagnostics

(* This intentionally mutates [analysis_state] in place. The analysis layer keeps
   package discovery tables as mutable hash tables so later requests can resolve
   files and modules without rebuilding package metadata. That makes this
   function unsafe in the usual functional sense: callers must provide a fresh
   per-server analysis state or accept that repeated calls append/overwrite
   package roots as a side effect. *)
let discover_subpackages_and_populate ~workspace_root
    ~(analysis_state : Analysis.Shared_types.state) ~server =
  let ( /+ ) = Filename.concat in

  let root_package =
    let root_path = workspace_root |> Uri.to_path in
    match Analysis.Packages.new_bs_package ~root_path with
    | Some p -> Some p
    | None ->
      (* TODO: Decide how the server should recover when the workspace has not
         been built yet, for example right after cloning a repository. Package
         discovery depends on generated build metadata, so most analysis
         features cannot work until that metadata exists.
         Options to evaluate:
         - Keep the server running and show a window message. In this case user should restart the server after build.
         - Stop the server after explaining that a build is required.
         - Re-initialize `analysis_state` after DidChangeWatchedFiles reports a
           new `.compiler.log` change.
         *)
      let message =
        Printf.sprintf
          "Failed to initialize the context for the project at %s. Try \
           building the project then restart the server"
          root_path
      in
      Server.log_message_notification ~kind:MessageType.Error message server;
      None
  in

  let resolve_node_modules_paths =
    match root_package with
    | Some {dependencies} ->
      let paths =
        dependencies
        |> List.filter_map (fun dep_name ->
               (* TODO: Resolve for deno and pnpm *)
               let node_modules =
                 (workspace_root |> Uri.to_path) /+ "node_modules"
               in
               let path = node_modules /+ dep_name in
               (* TODO: Replace with fs.ml module *)
               if Analysis.Files.exists path then Some (Unix.realpath path)
               else
                 let rescript = node_modules /+ "rescript" in
                 if Analysis.Files.exists rescript then
                   let real_path = Unix.realpath rescript /+ dep_name in
                   Some real_path
                 else None)
      in
      Some paths
    | None -> None
  in

  (match root_package with
  | Some root_package ->
    Hashtbl.add analysis_state.root_for_uri workspace_root
      root_package.root_path;
    Hashtbl.add analysis_state.packages_by_root root_package.root_path
      root_package
  | None -> ());

  (match resolve_node_modules_paths with
  | Some node_modules_paths ->
    node_modules_paths
    |> List.iter (fun node_module_path ->
           let uri = Uri.of_path node_module_path in
           match
             Analysis.Packages.new_bs_package ~root_path:node_module_path
           with
           | Some package ->
             Hashtbl.add analysis_state.root_for_uri uri package.root_path;
             Hashtbl.add analysis_state.packages_by_root package.root_path
               package
           | None -> ());
    ()
  | None -> ())
  |> ignore

let on_initialize (params : InitializeParams.t) (server : State.t Server.t) =
  let state = Server.state server in

  let diagnostics =
    Diagnostics.create ~diagnostics:(Diagnostics.empty ())
      ~send:(fun publish_diagnostics_params ->
        Server.notification
          (Server_notification.PublishDiagnostics publish_diagnostics_params)
          server)
  in

  let analysis_state = Analysis.Shared_types.create_state () in

  let workspace_root = Helpers.workspace_root_uri_of_initialize_params params in

  discover_subpackages_and_populate ~workspace_root ~analysis_state ~server;

  let compiler_config =
    analysis_state.packages_by_root |> Hashtbl.to_seq
    |> Seq.filter_map (fun (root_path, _) ->
           match Compiler_config.parse ~root:root_path ~fs:state.fs with
           | Ok config -> Some (Uri.of_path root_path, config)
           | Error _ ->
             (* NOTE: Surface malformed package configs to the client. We ignore
                them for now so initialization can continue with any remaining
                valid packages. *)
             None)
    |> Compiler_config.Uri_map.of_seq
  in

  let state =
    State.initialize state ~params ~diagnostics ~analysis_state ~compiler_config
  in
  let initialization_info = initialization () in
  (initialization_info, state)

let on_request (Client_request.E request) (server : State.t Server.t) =
  let load_full uri (state : State.t) =
    match state.status with
    | Initialized _ -> Helpers.load_full uri (State.analysis_state state)
    | Uninitialized -> None
  in

  let ok value = Ok (Client_request.yojson_of_result request value) in

  let error ?(code = Jsonrpc.Response.Error.Code.InternalError) message =
    Error (Jsonrpc.Response.Error.make ~message ~code ())
  in

  let state = Server.state server in

  match request with
  | Client_request.Initialize params ->
    let initialization_info, state = on_initialize params server in
    (ok initialization_info, state)
  | TextDocumentHover {position; textDocument = {uri}} ->
    let source = (Document_store.get ~uri state.store).text in
    let full = load_full uri state in
    let resp =
      Analysis.Commands.hover
        ~state:(State.analysis_state state)
        ~source ~kind_file:(Document.kind uri)
        ~pos:(position.line, position.character)
        ~debug:false
        ~supports_markdown_links:
          state.configuration.hover.support_markdown_links ~full
    in
    (ok resp, state)
  | TextDocumentDiagnostic {textDocument = {uri}} ->
    let diagnostics = State.diagnostics state |> Diagnostics.diagnostics in
    let items =
      Diagnostics.Uri_map.find_opt uri diagnostics |> Option.value ~default:[]
    in
    let resp =
      `RelatedFullDocumentDiagnosticReport
        (RelatedFullDocumentDiagnosticReport.create ~items ())
    in
    (ok resp, state)
  | TextDocumentCompletion {textDocument = {uri}; position} ->
    let source = (Document_store.get ~uri state.store).text in
    let full = load_full uri state in

    let resp =
      Analysis.Commands.completion
        ~state:(State.analysis_state state)
        ~debug:false ~source ~kind_file:(Document.kind uri)
        ~pos:(position.line, position.character)
        ~full
    in
    (ok (Some (`List resp)), state)
  | CompletionItemResolve item ->
    let resp =
      match (item.documentation, item.data) with
      (*
        documentation === null && item.data != null
        See https://github.com/rescript-lang/rescript-vscode/blob/2bc69d29ed92e19b14054952bafe9d4af7bd4c4b/server/src/server.ts#L958-L970
      *)
      | None, Some (`Assoc fields) -> (
        let file_path = List.assoc_opt "filePath" fields in
        let module_path = List.assoc_opt "modulePath" fields in
        match (file_path, module_path) with
        | Some (`String file_path), Some (`String module_path) ->
          let full = load_full (Uri.of_path file_path) state in
          let documentation =
            Analysis.Commands.completion_resolve
              ~state:(State.analysis_state state)
              ~full ~module_path
          in
          Some {item with documentation}
        | _ -> None)
      | _ -> None
    in
    (ok (resp |> Option.value ~default:item), state)
  | SignatureHelp {textDocument = {uri}; position} ->
    if state.configuration.signature_help.enable then
      let source = (Document_store.get ~uri state.store).text in
      let full = load_full uri state in
      let resp =
        match
          Analysis.Commands.signature_help
            ~state:(State.analysis_state state)
            ~source ~kind_file:(Document.kind uri)
            ~pos:(position.line, position.character)
            ~full
            ~allow_for_constructor_payloads:
              state.configuration.signature_help.for_constructor_payloads
            ~debug:false
        with
        | Some s -> s
        | None -> SignatureHelp.create ~signatures:[] ()
      in
      (ok resp, state)
    else (ok (SignatureHelp.create ~signatures:[] ()), state)
  | TextDocumentDefinition {textDocument = {uri}; position} ->
    let full = load_full uri state in
    let resp =
      match
        Analysis.Commands.definition
          ~state:(State.analysis_state state)
          ~full
          ~pos:(position.line, position.character)
          ~debug:false
      with
      | Some loc -> Some (`Location [loc])
      | None -> None
    in
    (ok resp, state)
  | TextDocumentTypeDefinition {textDocument = {uri}; position} ->
    let full = load_full uri state in
    let resp =
      match
        Analysis.Commands.type_definition
          ~state:(State.analysis_state state)
          ~full
          ~pos:(position.line, position.character)
          ~debug:false
      with
      | Some loc -> Some (`Location [loc])
      | None -> None
    in
    (ok resp, state)
  | TextDocumentReferences {textDocument = {uri}; position} ->
    let full = load_full uri state in
    let resp =
      Analysis.Commands.references
        ~state:(State.analysis_state state)
        ~full
        ~pos:(position.line, position.character)
        ~debug:false
    in
    (ok (Some resp), state)
  | DocumentSymbol {textDocument = {uri}} ->
    let source = (Document_store.get ~uri state.store).text in
    let resp =
      Analysis.Document_symbol.get_symbols ~source
        ~kind_file:(Document.kind uri)
    in
    (ok (Some (`DocumentSymbol resp)), state)
  | CodeAction
      {textDocument = {uri}; range = {start; end_}; context = {diagnostics}} ->
    let full = load_full uri state in
    let source = (Document_store.get ~uri state.store).text in
    let code_actions_from_compiler_log =
      Code_actions.From_diagnostics.get ~uri ~diagnostics ~source
    in
    let code_actions_from_analysis =
      Analysis.Xform.extract_code_actions
        ~state:(State.analysis_state state)
        ~path:(Uri.to_path uri)
        ~start_pos:(start.line, start.character)
        ~end_pos:(end_.line, end_.character)
        ~source ~kind_file:(Document.kind uri) ~full ~debug:false
    in
    let other_actions =
      let client_support_window_show_document =
        match (State.params state).capabilities.window with
        | Some {showDocument = Some {support}} -> support = true
        | _ -> false
      in

      let open_compiled_file =
        if client_support_window_show_document then
          Code_actions.Open_compiled_file.create ~uri ~state
        else []
      in
      let create_interface_file =
        Code_actions.Create_interface_file.create ~uri ~state
      in
      let switch_implementation_interface_file =
        if client_support_window_show_document then
          Code_actions.Switch_implementation_interface_file.create ~uri ~state
        else []
      in

      open_compiled_file @ create_interface_file
      @ switch_implementation_interface_file
    in
    let resp =
      code_actions_from_compiler_log @ code_actions_from_analysis
      @ other_actions
      |> List.map (fun ca -> `CodeAction ca)
    in
    (ok (Some resp), state)
  | TextDocumentCodeLens {textDocument = {uri}} ->
    if state.configuration.code_lens then
      let source = (Document_store.get ~uri state.store).text in
      let full = load_full uri state in
      let resp =
        Analysis.Hint.code_lens ~source ~kind_file:(Document.kind uri) ~full
          ~debug:false
      in
      (ok (resp |> Option.value ~default:[]), state)
    else (ok [], state)
  | InlayHint {textDocument = {uri}; range = {start; end_}} ->
    if state.configuration.inlay_hints.enable then
      let source = (Document_store.get ~uri state.store).text in
      let full = load_full uri state in
      let resp =
        Analysis.Hint.inlay
          ~state:(State.analysis_state state)
          ~source ~kind_file:(Document.kind uri) ~full
          ~pos:(start.line, end_.line)
          ~max_length:state.configuration.inlay_hints.max_length ~debug:false
      in
      (ok resp, state)
    else (ok None, state)
  | SemanticTokensFull {textDocument = {uri}} ->
    let source = (Document_store.get ~uri state.store).text in
    let resp =
      Analysis.Semantic_tokens.semantic_tokens ~source
        ~kind_file:(Document.kind uri)
    in
    (ok (Some resp), state)
  | TextDocumentRename {textDocument = {uri}; position; newName} ->
    let full = load_full uri state in
    let resp =
      match
        Analysis.Commands.rename
          ~state:(State.analysis_state state)
          ~full
          ~pos:(position.line, position.character)
          ~new_name:newName ~debug:false
      with
      | Some we -> we
      | None -> WorkspaceEdit.create ()
    in
    (ok resp, state)
  | TextDocumentPrepareRename {textDocument = {uri}; position} ->
    let full = load_full uri state in
    let resp =
      match
        Analysis.Commands.prepare_rename ~full
          ~pos:(position.line, position.character)
          ~debug:false
      with
      | Some {range} -> Some range
      | None -> None
    in
    (ok resp, state)
  | TextDocumentFormatting {textDocument = {uri}} -> (
    match Formatter.run ~uri ~state with
    | Ok text_edit -> (ok (Some text_edit), state)
    | Error message -> (error message, state))
  | ExecuteCommand {command; arguments} -> (
    let open Execute_commands in
    match
      List.assoc_opt command
        [
          (Open_compiled.name, Open_compiled.execute);
          (Create_interface.name, Create_interface.execute);
          ( Switch_implementation_interface.name,
            Switch_implementation_interface.execute );
          (Dump_server_state.name, Dump_server_state.execute);
          (Dump_cmt.name, Dump_cmt.execute);
        ]
    with
    | Some execute -> (
      match execute ~arguments server with
      | Ok None -> (ok `Null, state)
      | Ok (Some response) -> (ok response, state)
      | Error message -> (error message, state))
    | None ->
      ( error
          (Printf.sprintf
             "Unknown command %s for workspace/executeCommand request" command),
        state ))
  | Shutdown -> (ok (), state)
  | DebugTextDocumentGet _ | DebugEcho _ | WorkspaceSymbol _
  | CodeActionResolve _ | TextDocumentColor _ | TextDocumentColorPresentation _
  | TextDocumentCodeLensResolve _ | TextDocumentHighlight _
  | TextDocumentFoldingRange _ | TextDocumentLinkResolve _ | TextDocumentLink _
  | WillSaveWaitUntilTextDocument _ | TextDocumentRangeFormatting _
  | TextDocumentOnTypeFormatting _ | SelectionRange _
  | TextDocumentImplementation _ | SemanticTokensDelta _ | TextDocumentMoniker _
  | TextDocumentPrepareCallHierarchy _ | CallHierarchyIncomingCalls _
  | CallHierarchyOutgoingCalls _ | SemanticTokensRange _ | LinkedEditingRange _
  | WillCreateFiles _ | WillRenameFiles _ | WillDeleteFiles _
  | InlayHintResolve _ | TextDocumentInlineCompletion _
  | TextDocumentInlineValue _ | WorkspaceSymbolResolve _ | WorkspaceDiagnostic _
  | TextDocumentRangesFormatting _ | TextDocumentPrepareTypeHierarchy _
  | TypeHierarchySupertypes _ | TypeHierarchySubtypes _
  | TextDocumentDeclaration _ ->
    (error "Request not supported yet!", state)
  | UnknownRequest {meth; params} -> (
    let open Custom_requests in
    match
      List.assoc_opt meth
        [
          (Create_interface_file.meth, Create_interface_file.on_request);
          (Open_compiled_file.meth, Open_compiled_file.on_request);
        ]
    with
    | Some on_request -> (on_request ~params ~state, state)
    | None ->
      ( error ?code:(Some Jsonrpc.Response.Error.Code.InvalidRequest)
          (Printf.sprintf "Unknown request %s" meth),
        state ))

let on_notification notification (server : State.t Server.t) =
  let state = Server.state server in

  match notification with
  | Client_notification.TextDocumentDidOpen
      {textDocument = {uri; text; version; _}} ->
    let store = Document_store.add ~uri ~text ~version state.store in

    let diagnostics =
      State.diagnostics state
      |> get_updated_diagnostics_from_log state
      |> Diagnostics.update_syntax ~uri
           ~new_diagnostics:
             (Analysis.Diagnostics.document_syntax
                ~source:(Document_store.get ~uri store).text
                ~kind_file:(Document.kind uri))
    in
    diagnostics |> Diagnostics.send;

    {state with store} |> State.update_diagnostics diagnostics
  | TextDocumentDidChange {contentChanges; textDocument = {uri; version}} ->
    let store =
      match List.rev contentChanges with
      | {text} :: _ -> Document_store.update ~uri ~text ~version state.store
      | [] -> state.store
    in
    let diagnostics =
      State.diagnostics state
      |> get_updated_diagnostics_from_log state
      |> Diagnostics.update_syntax ~uri
           ~new_diagnostics:
             (Analysis.Diagnostics.document_syntax
                ~source:(Document_store.get ~uri store).text
                ~kind_file:(Document.kind uri))
    in

    (* Syntax diagnostics for open buffers are served by textDocument/diagnostic.
       Do not push them here as publishDiagnostics too, otherwise clients that
       support pull diagnostics can show duplicates on every change. Ensure the
       changed URI is still published with an empty/non-syntax result so any
       previously pushed syntax diagnostics are cleared. *)
    diagnostics
    |> Diagnostics.send ~include_syntax:false ~force_publish_uris:[uri];
    {state with store} |> State.update_diagnostics diagnostics
  | TextDocumentDidClose {textDocument = {uri; _}} ->
    let store = Document_store.remove ~uri state.store in
    let diagnostics =
      State.diagnostics state
      |> Diagnostics.clear_syntax ~uri
      |> get_updated_diagnostics_from_log state
    in
    diagnostics |> Diagnostics.send;
    {state with store} |> State.update_diagnostics diagnostics
  | Initialized ->
    (* Register dynamic file watchers for compiler log files.
       ReScript writes one .compiler.log per build root. In monorepos,
       .sourcedirs.json contains the build_root entries for each subpackage,
       so use it to watch every generated compiler log and refresh diagnostics
       when any of them changes. *)
    let watchers =
      [WatchKind.Create; Change; Delete]
      |> List.map (fun kind ->
             FileSystemWatcher.create
               ~kind
                 (* NOTE: Clients dont send notification `workspace/didChangeWatchedFiles`
                    when pattern is a relative path `lib/bs/.compiler.log` of full path
                    `{workspace_root}/lib/bs/.compiler.log`. The glob should start with `**`
                  *)
               ~globPattern:(`Pattern ("**/lib/bs/" ^ Constants.compiler_log))
               ())
    in

    let registerOptions =
      DidChangeWatchedFilesRegistrationOptions.create ~watchers
      |> DidChangeWatchedFilesRegistrationOptions.yojson_of_t
    in
    let registration =
      Registration.create ~id:"rescript_file_watchers"
        ~method_:"workspace/didChangeWatchedFiles" ~registerOptions ()
    in
    let params = RegistrationParams.create ~registrations:[registration] in
    Server.request (Server_request.ClientRegisterCapability params) server;

    state
  | DidChangeWatchedFiles _ ->
    (* Do not limit diagnostics to the path reported by
       DidChangeWatchedFilesParams. In monorepos, a build in one subpackage
       can change diagnostics that should be shown for files in another
       subpackage. Re-read every compiler log listed in .sourcedirs.json so
       stale errors are cleared and cross-package diagnostics stay in sync. *)
    let diagnostics =
      State.diagnostics state |> get_updated_diagnostics_from_log state
    in
    diagnostics |> Diagnostics.send;

    Server.notification
      (Server_notification.UnknownNotification
         (Jsonrpc.Notification.create ~method_:"rescript/compilationFinished" ()))
      server;

    (match (State.params state).capabilities.workspace with
    | Some {codeLens = Some {refreshSupport = Some true}} ->
      Server.request Server_request.CodeLensRefresh server
    | _ -> ());

    state |> State.update_diagnostics diagnostics
  | ChangeConfiguration _ ->
    (* workspace/didChangeConfiguration only signals that settings may have
       changed. The LSP configuration model is pull-based, so the server should
       ignore this notification payload and fetch the scoped settings it needs
       with workspace/configuration.

       See https://github.com/microsoft/language-server-protocol/issues/567#issuecomment-448538082
    *)
    Server.request
      (Server_request.WorkspaceConfiguration
         (ConfigurationParams.create
          (* NOTE: The client decides how to map that section to its own configuration store.
             Behavior I observed using `rescript.settings`:
              - Neovim send `null`
              - Zed send `null`
              - VSCode sends the correct data layout
             The best approach was to increase the scope to `rescript` and write a small middleware in VSCode extension.
           *)
            ~items:[ConfigurationItem.create ~section:"rescript" ()]))
      server;
    state
  | Exit -> state
  | ChangeWorkspaceFolders _ | CancelRequest _ | DidSaveTextDocument _
  | DidCreateFiles _ | DidDeleteFiles _ | DidRenameFiles _
  | WillSaveTextDocument _ | WorkDoneProgressCancel _ | WorkDoneProgress _
  | NotebookDocumentDidOpen _ | NotebookDocumentDidChange _
  | NotebookDocumentDidSave _ | NotebookDocumentDidClose _ | SetTrace _ ->
    state
  | UnknownNotification {method_} ->
    Server.log_message_notification ~kind:MessageType.Error
      ("Unknown notication " ^ method_)
      server;
    state

(* TODO: Revisit this *)
let on_response
    (Server.Response_result (request, result) : Server.response_result)
    (server : State.t Server.t) =
  let request_error_message = function
    | Server.Response_error error -> error.message
    | Server.Decode_error exn -> Printexc.to_string exn
  in

  let state = Server.state server in

  match (request, result) with
  | Server_request.WorkspaceConfiguration _, result -> (
    match result with
    | Ok [settings] -> (
      match Configuration.of_yojson settings with
      | Ok configuration -> {state with configuration}
      | Error _ ->
        Server.log_message_notification ~kind:MessageType.Error
          ("Failed to parse rescript settings: "
          ^ Yojson.Safe.pretty_to_string settings)
          server;
        state)
    | Ok _ ->
      Server.log_message_notification ~kind:MessageType.Error
        "Invalid rescript settings. The server not received a list with one \
         element"
        server;
      state
    | Error err ->
      Server.log_message_notification ~kind:MessageType.Error
        ("Error on response of workspace/configuration request: "
       ^ request_error_message err)
        server;
      state)
  | _ -> state

let listen ~input ~output ~fs =
  let state =
    State.create ~store:(Document_store.create ())
      ~configuration:Configuration.default ~fs
  in
  Server.listen ~input ~output ~on_request ~on_notification ~on_response ~state

let initialization (_client_capabilities : Lsp.Types.ClientCapabilities.t) =
  let open Lsp.Types in
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create ~openClose:true
         ~change:TextDocumentSyncKind.Full ~willSave:false
         ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
         ~willSaveWaitUntil:false ())
  in
  let capabilities =
    ServerCapabilities.create ~textDocumentSync ~hoverProvider:(`Bool true) ()
  in
  let serverInfo =
    let version = "2.0.0-aplha.1" in
    InitializeResult.create_serverInfo ~name:"rescript-language-server" ~version
      ()
  in
  InitializeResult.create ~capabilities ~serverInfo ()

let get_updated_diagnostics (state : State.t) =
  let workspace_root = State.workspace_root state in
  let diagnostics =
    Compiler.collect_diagnostics_from_log_using_source_dirs workspace_root state
    |> Diagnostics.convert_to_lsp workspace_root
  in
  Diagnostics.set ~diagnostics (State.diagnostics state)

let on_initialize (params : Lsp.Types.InitializeParams.t)
    (server : State.t Server.t) =
  let state = Server.state server in

  let diagnostics =
    Diagnostics.create ~diagnostics:Diagnostics.Uri_map.empty
      ~send:(fun publish_diagnostics ->
        publish_diagnostics
        |> List.iter (fun publish_diagnostic_params ->
               Server.notification
                 (Lsp.Server_notification.PublishDiagnostics
                    publish_diagnostic_params) server))
  in
  let state = State.initialize state ~params ~diagnostics in
  let initialization_info = initialization params.capabilities in
  (initialization_info, state)

let on_request (Lsp.Client_request.E request) (server : State.t Server.t) =
  let state = Server.state server in
  let ok value = Ok (Lsp.Client_request.yojson_of_result request value) in
  match request with
  | Lsp.Client_request.Initialize params ->
    let initialization_info, state = on_initialize params server in
    (ok initialization_info, state)
  | Shutdown -> (ok (), state)
  | TextDocumentHover {position; textDocument = {uri}; _} ->
    (ok (Hover.create ~position ~uri server), state)
  | _ ->
    let err =
      Jsonrpc.Response.Error.make
        ~code:Jsonrpc.Response.Error.Code.MethodNotFound
        ~message:"Request method not supported" ()
    in
    (Error err, state)

let on_notification notification (server : State.t Server.t) =
  let state = Server.state server in

  match notification with
  | Lsp.Client_notification.TextDocumentDidOpen
      {textDocument = {uri; text; version; _}} ->
    let store = Document_store.open_document ~uri ~text ~version state.store in
    let diagnostics = get_updated_diagnostics state in
    diagnostics |> Diagnostics.send;
    {state with store} |> State.update_diagnostics diagnostics
  | TextDocumentDidChange _ -> state
  | TextDocumentDidClose {textDocument = {uri; _}} ->
    let store = Document_store.remove_document ~uri state.store in
    let diagnostics = get_updated_diagnostics state in
    diagnostics |> Diagnostics.send;
    {state with store} |> State.update_diagnostics diagnostics
  | Initialized ->
    let open Lsp.Types in
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
    Server.request (Lsp.Server_request.ClientRegisterCapability params) server;

    state
  | DidChangeWatchedFiles _ ->
    (* Do not limit diagnostics to the path reported by
       DidChangeWatchedFilesParams. In monorepos, a build in one subpackage
       can change diagnostics that should be shown for files in another
       subpackage. Re-read every compiler log listed in .sourcedirs.json so
       stale errors are cleared and cross-package diagnostics stay in sync. *)
    let diagnostics = get_updated_diagnostics state in
    diagnostics |> Diagnostics.send;
    state |> State.update_diagnostics diagnostics
  | Exit -> state
  | _ -> state

let main () =
  Eio_main.run (fun env ->
      let state = State.create ~store:(Document_store.create ()) ~env in
      Server.listen ~input:env#stdin ~output:env#stdout ~on_request
        ~on_notification ~state ~env)

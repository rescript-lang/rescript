let initialization (client_capabilities : Lsp.Types.ClientCapabilities.t) =
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

let on_initialize (params : Lsp.Types.InitializeParams.t) (state : State.t) =
  (* TODO:
    * Find root project (rescript.json, package.json) using InitializeParams.workspaceFolders and save in State.t
      * See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeParams
      * If not found rescript.json kill the server?
    * Save initializationOptions in State.t
      * This options are: askToStartBuild, codeLens.enable, inlayHints.enable, etc..
    * Collect compiler diagnostics (syntax and type)?
  *)
  let diagnostics = Diagnostics.create () in
  let initialization_info = initialization params.capabilities in
  let state = State.initialize state ~params ~diagnostics in
  (initialization_info, state)

let on_request (Lsp.Client_request.E request) (server : State.t Server.t) =
  let state = Server.state server in
  let ok value = Ok (Lsp.Client_request.yojson_of_result request value) in
  match request with
  | Lsp.Client_request.Initialize params ->
    let initialization_info, state = on_initialize params state in
    (ok initialization_info, state)
  | Shutdown -> (ok (), state)
  | TextDocumentHover {position; textDocument = {uri}} ->
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
    {state with store}
  (* | TextDocumentDidChange {textDocument = {uri; version; _}; contentChanges}
    -> (
    match List.rev contentChanges with
    | {text; _} :: _ -> state
    | [] -> state) *)
  | TextDocumentDidClose {textDocument = {uri; _}} ->
    (* TODO:
     * remove state diagnostics
     * send updated diagnostics?
     *)
    let store = Document_store.remove_document ~uri state.store in
    {state with store}
  | Exit -> state
  | _ -> state

let main () =
  Eio_main.run (fun env ->
      let state = State.create ~store:(Document_store.create ()) in
      Server.listen ~input:env#stdin ~output:env#stdout ~on_request
        ~on_notification ~state ~env)

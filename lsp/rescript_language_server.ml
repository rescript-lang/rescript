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

let show_message server message =
  Server.notification server
    (Lsp.Server_notification.ShowMessage
       (Lsp.Types.ShowMessageParams.create ~type_:Info ~message))

let on_initialize (params : Lsp.Types.InitializeParams.t) state =
  (* TODO: collect compiler diagnostics and notify client? *)
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
    let current_file = (Document_store.get_document ~uri state.store).text in
    show_message server (Lsp.Types.DocumentUri.to_path uri);
    let _res = Hover.create ~position ~uri ~current_file in
    (ok _res, state)
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
  | TextDocumentDidChange {textDocument = {uri; version; _}; contentChanges}
    -> (
    match List.rev contentChanges with
    | {text; _} :: _ -> state
    | [] -> state)
  | TextDocumentDidClose {textDocument = {uri; _}} ->
    let store = Document_store.remove_document ~uri state.store in
    (* TODO: 
     * remove state diagnostics
     * send updated diagnostics?
     *)

    {state with store}
  | Exit -> exit 0
  | _ -> state

let main () =
  Eio_main.run @@ fun env ->
  let stdin = Eio.Stdenv.stdin env in
  let stdout = Eio.Stdenv.stdout env in
  Server.listen ~input:stdin ~output:stdout ~on_request ~on_notification
    ~state:(State.create ~store:(Document_store.create ()))

let () = main ()

let initialization =
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

let on_request (Lsp.Client_request.E request) =
  let open Lsp.Types in
  let ok value = Ok (Lsp.Client_request.yojson_of_result request value) in
  match request with
  | Lsp.Client_request.Initialize _ -> ok initialization
  | Shutdown -> ok ()
  | TextDocumentHover _ ->
    let hover =
      Lsp.Types.Hover.create
        ~contents:
          (`MarkupContent
             (MarkupContent.create ~kind:MarkupKind.Markdown
                ~value:"# Hover working!!!"))
        ()
    in
    ok (Some hover)
  | _ ->
    Error
      (Jsonrpc.Response.Error.make
         ~code:Jsonrpc.Response.Error.Code.MethodNotFound
         ~message:"Request method not supported" ())

let on_notification state _channel notification =
  match notification with
  | Lsp.Client_notification.Initialized -> state
  | TextDocumentDidOpen {textDocument = {uri; text; version; _}} ->
    State.open_document state ~uri ~text ~version
  | TextDocumentDidChange {textDocument = {uri; version; _}; contentChanges}
    -> (
    match List.rev contentChanges with
    | {text; _} :: _ -> State.update_document state ~uri ~text ~version
    | [] -> state)
  | TextDocumentDidClose {textDocument = {uri; _}} ->
    (* let uri = Lsp.Uri.to_string textDocument.uri in *)
    State.close_document state ~uri
  | Exit -> exit 0
  | _ -> state

let main () =
  Eio_main.run @@ fun env ->
  let stdin = Eio.Stdenv.stdin env in
  let stdout = Eio.Stdenv.stdout env in
  Server.listen ~input:stdin ~output:stdout ~on_request ~on_notification
    ~state:State.empty

let () = main ()

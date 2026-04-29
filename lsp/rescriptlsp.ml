module Io : sig
  type 'a t

  val return : 'a -> 'a t
  val raise : exn -> 'a t
  val await : 'a t -> 'a
  val async : (sw:Eio.Switch.t -> ('a, exn) result) -> 'a t

  module O : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end = struct
  type 'a t = sw:Eio.Switch.t -> ('a, exn) result Eio.Promise.t

  let await t = Eio.Switch.run @@ fun sw -> Eio.Promise.await_exn (t ~sw)
  let return value ~sw:_ = Eio.Promise.create_resolved (Ok value)
  let error desc ~sw:_ = Eio.Promise.create_resolved (Error desc)

  let async f ~sw =
    let promise, resolver = Eio.Promise.create () in
    ( Eio.Fiber.fork ~sw @@ fun () ->
      try
        let result = f ~sw in
        Eio.Promise.resolve resolver result
      with exn -> Eio.Promise.resolve resolver @@ Error exn );
    promise

  let bind t f =
    async @@ fun ~sw ->
    match Eio.Promise.await (t ~sw) with
    | Ok value -> Eio.Promise.await @@ f value ~sw
    | Error desc -> Error desc

  let raise = error

  module O = struct
    let ( let+ ) x f = bind x @@ fun value -> return @@ f value
    let ( let* ) = bind
  end
end

module Chan : sig
  type input
  type output

  val of_source : [> Eio__Flow.source_ty] Eio.Resource.t -> input
  val with_sink : [> Eio__Flow.sink_ty] Eio.Resource.t -> (output -> 'a) -> 'a

  val read_line : input -> string option Io.t
  val read_exactly : input -> int -> string option Io.t
  val write : output -> string list -> unit Io.t
end = struct
  type input = {mutex: Eio.Mutex.t; buf: Eio.Buf_read.t}
  type output = {mutex: Eio.Mutex.t; buf: Eio.Buf_write.t}

  let initial_size = 1024
  let max_size = 1024 * 1024

  let of_source source : input =
    let mutex = Eio.Mutex.create () in
    let buf = Eio.Buf_read.of_flow ~initial_size ~max_size source in
    {mutex; buf}

  let with_sink sink f =
    let mutex = Eio.Mutex.create () in
    Eio.Buf_write.with_flow ~initial_size sink @@ fun buf -> f {mutex; buf}

  let read_line (input : input) =
    Io.async @@ fun ~sw:_ ->
    Eio.Mutex.use_rw ~protect:true input.mutex @@ fun () ->
    if Eio.Buf_read.eof_seen input.buf then Ok None
    else
      match Eio.Buf_read.line input.buf with
      | line -> Ok (Some line)
      | exception End_of_file -> Ok None

  let read_exactly (input : input) size =
    Io.async @@ fun ~sw:_ ->
    Eio.Mutex.use_rw ~protect:true input.mutex @@ fun () ->
    if Eio.Buf_read.eof_seen input.buf then Ok None
    else
      match Eio.Buf_read.take size input.buf with
      | data -> Ok (Some data)
      | exception End_of_file -> Ok None

  let write (output : output) (str : string list) =
    Io.async @@ fun ~sw:_ ->
    Eio.Mutex.use_rw ~protect:true output.mutex @@ fun () ->
    Ok (List.iter (Eio.Buf_write.string output.buf) str)
end

module Lsp_Io = Lsp.Io.Make (Io) (Chan)

let request_of_jsonrpc request =
  match Lsp.Client_request.of_jsonrpc request with
  | Ok request -> request
  | Error error -> raise (Lsp.Io.Error error)

let notification_of_jsonrpc notification =
  match Lsp.Client_notification.of_jsonrpc notification with
  | Ok notification -> notification
  | Error error -> raise (Lsp.Io.Error error)

let respond channel response =
  Io.await @@ Lsp_Io.write channel @@ Response response

let rec input_loop ~input with_ =
  match Io.await @@ Lsp_Io.read input with
  | Some packet ->
    let () = with_ packet in
    input_loop ~input with_
  | exception exn -> raise exn
  | None -> ()

let listen ~input ~output ~on_request ~on_notification =
  let handle_request channel request =
    respond channel (on_request channel request)
  in
  let handle_notification channel notification =
    on_notification channel (notification_of_jsonrpc notification)
  in
  let input = Chan.of_source input in
  Chan.with_sink output @@ fun channel ->
  input_loop ~input @@ fun packet ->
  match packet with
  | Notification notification -> handle_notification channel notification
  | Request request -> handle_request channel request
  | Batch_call calls ->
    List.iter
      (fun call ->
        match call with
        | `Request request -> handle_request channel request
        | `Notification notification -> handle_notification channel notification)
      calls
  | Response _ -> raise (Lsp.Io.Error "unexpected response")
  | Batch_response _ -> raise (Lsp.Io.Error "unexpected batch response")

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
    let version = "experimental" in
    InitializeResult.create_serverInfo ~name:"rescriptlsp" ~version ()
  in
  InitializeResult.create ~capabilities ~serverInfo ()

let on_request _channel (jsonrpc_request : Jsonrpc.Request.t) : Jsonrpc.Response.t =
  let result =
    let (E request) = request_of_jsonrpc jsonrpc_request in
    match request with
    | Lsp.Client_request.Initialize _ ->
      Ok (Lsp.Client_request.yojson_of_result request initialization)
    | Shutdown -> Ok (Lsp.Client_request.yojson_of_result request ())
    | TextDocumentHover _ ->
      Ok (Lsp.Client_request.yojson_of_result request None)
    | _ ->
      Error
        (Jsonrpc.Response.Error.make
           ~code:Jsonrpc.Response.Error.Code.MethodNotFound
           ~message:"Method not supported" ())
  in
  Jsonrpc.Response.{id = jsonrpc_request.id; result}

let on_notification _channel notification =
  match notification with
  | Lsp.Client_notification.Initialized -> ()
  | TextDocumentDidOpen _ -> ()
  | TextDocumentDidChange _ -> ()
  | Exit -> exit 0
  | _ -> ()

let main () =
  Eio_main.run @@ fun env ->
  let stdin = Eio.Stdenv.stdin env in
  let stdout = Eio.Stdenv.stdout env in
  listen ~input:stdin ~output:stdout ~on_request ~on_notification

let () = main ()

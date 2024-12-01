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

  (* eio *)
  (* val of_source : 'a Eio.Flow.source -> input *)
  (* val with_sink : 'a Eio.Flow.sink -> (output -> 'a) -> 'a *)
  val of_source : [> Eio__Flow.source_ty] Eio.Resource.t -> input
  val with_sink :
    [> Eio__Flow.sink_ty] Eio.Resource.t -> (output -> output) -> output

  (* lsp *)
  val read_line : input -> string option Io.t
  val read_exactly : input -> int -> string option Io.t
  val write : output -> string list -> unit Io.t
end = struct
  type input = {mutex: Eio.Mutex.t; buf: Eio.Buf_read.t}
  type output = {mutex: Eio.Mutex.t; buf: Eio.Buf_write.t}

  (* TODO: magic numbers *)
  let initial_size = 1024
  let max_size = 1024 * 1024

  let of_source source : input =
    let mutex = Eio.Mutex.create () in
    let buf = Eio.Buf_read.of_flow ~initial_size ~max_size source in
    {mutex; buf}

  let with_sink sink f : output =
    let mutex = Eio.Mutex.create () in
    Eio.Buf_write.with_flow ~initial_size sink @@ fun buf -> f @@ {mutex; buf}

  let read_line (input : input) =
    (* let { mutex; buf } = input in *)
    Io.async @@ fun ~sw:_ ->
    (* TODO: what this protect does? *)
    Eio.Mutex.use_rw ~protect:true input.mutex @@ fun () ->
    match Eio.Buf_read.eof_seen input.buf with
    | true -> Ok None
    | false -> Ok (Some (Eio.Buf_read.line input.buf))

  let read_exactly (input : input) size =
    Io.async @@ fun ~sw:_ ->
    Eio.Mutex.use_rw ~protect:true input.mutex @@ fun () ->
    match Eio.Buf_read.eof_seen input.buf with
    | true -> Ok None
    | false -> Ok (Some (Eio.Buf_read.take size input.buf))

  let write (output : output) (str : string list) =
    Io.async @@ fun ~sw:_ ->
    Eio.Mutex.use_rw ~protect:true output.mutex @@ fun () ->
    (* TODO(@aspeddro): Remove List.hd? *)
    Ok (Eio.Buf_write.string output.buf (List.hd str))
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

type channel = Chan.output

type on_request = {
  f:
    'response.
    channel ->
    'response Lsp.Client_request.t ->
    ('response, Jsonrpc.Response.Error.t) result;
}

let notify channel notification =
  (* TODO: fork here *)
  (* TODO: buffering and async? *)
  let notification = Lsp.Server_notification.to_jsonrpc notification in
  Io.await @@ Lsp_Io.write channel @@ Notification notification

let respond channel response =
  Io.await @@ Lsp_Io.write channel @@ Response response

let rec input_loop ~input ~output with_ =
  (* TODO: buffering and async handling *)
  match Io.await @@ Lsp_Io.read input with
  | Some packet ->
    let () = with_ packet in
    input_loop ~input ~output with_
  | exception exn -> (* TODO: handle this exception *) raise exn
  | None ->
    (* TODO: this means EOF right? *)
    ()

let listen ~input ~output ~on_request ~on_notification =
  let on_request channel request =
    (* TODO: error handling *)
    let result =
      let (E request) = request_of_jsonrpc request in
      match on_request.f channel request with
      | Ok result -> Ok (Lsp.Client_request.yojson_of_result request result)
      | Error _error as error -> error
    in
    let response = Jsonrpc.Response.{id = request.id; result} in
    respond channel response
  in
  let on_notification channel notification =
    let notification = notification_of_jsonrpc notification in
    on_notification channel notification
  in

  let input = Chan.of_source input in
  let a = Chan.with_sink output in
  Chan.with_sink output @@ fun channel ->
  input_loop ~input ~output @@ fun packet ->
  (* TODO: make this async? *)
  match packet with
  | Notification notification -> on_notification channel notification
  | Request request -> on_request channel request
  | Batch_call calls ->
    (* TODO: what if one fails? It should not prevents the others *)
    List.iter
      (fun call ->
        match call with
        | `Request request -> on_request channel request
        | `Notification notification -> on_notification channel notification)
      calls
  (* TODO: can the server receive a response?
      Yes but right now it will not be supported *)
  | Response _ -> raise (Lsp.Io.Error "")
  | Batch_response _ -> raise (Lsp.Io.Error "")

(* open Lsp_error *)

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

let main () = ()

let () = print_endline "rescript-lsp lolll"

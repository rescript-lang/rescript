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

let notification_of_jsonrpc notification =
  match Lsp.Client_notification.of_jsonrpc notification with
  | Ok notification -> notification
  | Error error -> raise (Lsp.Io.Error error)

module Request_id = struct
  type t = Jsonrpc.Id.t

  let equal = Jsonrpc.Id.equal
  let hash = Jsonrpc.Id.hash
end

module Request_id_table = Hashtbl.Make (Request_id)

type request_error =
  | Response_error of Jsonrpc.Response.Error.t
  | Decode_error of exn

type response_result =
  | Response_result :
      'a Lsp.Server_request.t * ('a, request_error) result
      -> response_result

type pending_request = Pending : 'a Lsp.Server_request.t -> pending_request

type request_context = {
  mutable next_id: int;
  pending: pending_request Request_id_table.t;
}

type 'a t = {channel: Chan.output; state: 'a; request_context: request_context}

let state t = t.state

let respond server response =
  Io.await @@ Lsp_Io.write server.channel @@ Response response

let notification notification server =
  let notification = Lsp.Server_notification.to_jsonrpc notification in
  Io.await @@ Lsp_Io.write server.channel @@ Notification notification

let request request server =
  let id = `Int server.request_context.next_id in
  server.request_context.next_id <- server.request_context.next_id + 1;
  Request_id_table.add server.request_context.pending id (Pending request);
  let request = Lsp.Server_request.to_jsonrpc_request request ~id in
  try Io.await @@ Lsp_Io.write server.channel @@ Request request
  with exn ->
    Request_id_table.remove server.request_context.pending id;
    raise exn

let handle_response (response : Jsonrpc.Response.t) server =
  match
    Request_id_table.find_opt server.request_context.pending response.id
  with
  | None -> None
  | Some (Pending request) -> (
    Request_id_table.remove server.request_context.pending response.id;
    match response.result with
    | Ok json ->
      let result =
        match Lsp.Server_request.response_of_json request json with
        | response -> Ok response
        | exception exn -> Error (Decode_error exn)
      in
      Some (Response_result (request, result))
    | Error err ->
      let result = Error (Response_error err) in
      Some (Response_result (request, result)))

let log_message_notification ?(kind = Lsp.Types.MessageType.Debug) message
    server =
  notification
    (Lsp.Server_notification.LogMessage
       (Lsp.Types.LogMessageParams.create ~type_:kind ~message))
    server

let show_message_notification ?(kind = Lsp.Types.MessageType.Info) message
    server =
  notification
    (Lsp.Server_notification.ShowMessage
       (Lsp.Types.ShowMessageParams.create ~type_:kind ~message))
    server

type lifecycle = Awaiting_initialize | Running | Shutdown_requested

let error_response ~id ~code ~message =
  let err = Jsonrpc.Response.Error.make ~code ~message () in
  Jsonrpc.Response.{id; result = Error err}

let is_initialize_request (request : Jsonrpc.Request.t) =
  request.method_ = "initialize"

let is_shutdown_request (request : Jsonrpc.Request.t) =
  request.method_ = "shutdown"

let is_exit_notification (notification : Jsonrpc.Notification.t) =
  notification.method_ = "exit"

let exit_from_lifecycle lifecycle =
  let exit_code =
    match lifecycle with
    | Shutdown_requested -> 0
    | _ -> 1
  in
  exit exit_code

let rec input_loop ~input ~state with_ =
  match Io.await @@ Lsp_Io.read input with
  | Some packet ->
    let state = with_ state packet in
    input_loop ~input ~state with_
  | exception _ -> failwith "Server.input_loop"
  | None -> ()

let listen ~input ~output ~on_request ~on_notification ~on_response ~state =
  let lifecycle = ref Awaiting_initialize in
  let handle_request server request =
    match !lifecycle with
    | Awaiting_initialize when not (is_initialize_request request) ->
      respond server
        (error_response ~id:request.id
           ~code:Jsonrpc.Response.Error.Code.ServerNotInitialized
           ~message:"Server has not received an initialize request");
      server.state
    | Running when is_initialize_request request ->
      respond server
        (error_response ~id:request.id
           ~code:Jsonrpc.Response.Error.Code.InvalidRequest
           ~message:"Server has already been initialized");
      server.state
    | Shutdown_requested ->
      respond server
        (error_response ~id:request.id
           ~code:Jsonrpc.Response.Error.Code.InvalidRequest
           ~message:"Server has already received a shutdown request");
      server.state
    | Awaiting_initialize | Running ->
      let response, state =
        match Lsp.Client_request.of_jsonrpc request with
        | Error message ->
          let code = Jsonrpc.Response.Error.Code.InvalidParams in
          let err = Jsonrpc.Response.Error.make ~code ~message () in
          (Jsonrpc.Response.{id = request.id; result = Error err}, state)
        | Ok packed ->
          let result, state = on_request packed server in
          (Jsonrpc.Response.{id = request.id; result}, state)
      in
      respond server response;
      (match response.result with
      | Ok _ when is_initialize_request request -> lifecycle := Running
      | Ok _ when is_shutdown_request request -> lifecycle := Shutdown_requested
      | Ok _ | Error _ -> ());
      state
  in
  let handle_notification server notification =
    if is_exit_notification notification then exit_from_lifecycle !lifecycle
    else
      match !lifecycle with
      | Awaiting_initialize | Shutdown_requested -> server.state
      | Running -> on_notification (notification_of_jsonrpc notification) server
  in
  let input = Chan.of_source input in
  Chan.with_sink output (fun channel ->
      let request_context =
        {next_id = 1; pending = Request_id_table.create 16}
      in
      input_loop ~input ~state (fun state packet ->
          let server = {channel; state; request_context} in
          match packet with
          | Notification notification -> handle_notification server notification
          | Request request -> handle_request server request
          | Batch_call calls ->
            List.fold_left
              (fun state call ->
                let server = {channel; state; request_context} in
                match call with
                | `Request request -> handle_request server request
                | `Notification notification ->
                  handle_notification server notification)
              state calls
          | Response response -> (
            match handle_response response server with
            | Some response -> on_response response server
            | None -> state)
          | Batch_response responses ->
            List.fold_left
              (fun state response ->
                let server = {channel; state; request_context} in
                match handle_response response server with
                | Some response -> on_response response server
                | None -> state)
              state responses))

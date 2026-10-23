let ( // ) = Filename.concat
let ( / ) = Eio.Path.( / )
let executable = "rescript-language-server"

let source_root () =
  Sys.getenv_opt "DUNE_SOURCEROOT" |> Option.value ~default:(Sys.getcwd ())

let rec mkdir_p path =
  if path = "" || Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755)

let write_file path content =
  mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let local_platform_bin name =
  let bin_name = name ^ ".exe" in
  let packages_dir = source_root () // "packages" // "@rescript" in
  ["linux-x64"; "linux-arm64"; "darwin-x64"; "darwin-arm64"; "win32-x64"]
  |> List.find_map (fun platform ->
      let path = packages_dir // platform // "bin" // bin_name in
      if Sys.file_exists path then Some path else None)
  |> Option.value ~default:bin_name

let with_chdir dir f =
  let cwd = Sys.getcwd () in
  Fun.protect
    ~finally:(fun () -> Sys.chdir cwd)
    (fun () ->
      Sys.chdir dir;
      f ())

let with_env_var name value env =
  let prefix = name ^ "=" in
  let without_name =
    env |> Array.to_list
    |> List.filter (fun item -> not (String.starts_with ~prefix item))
  in
  Array.of_list ((prefix ^ value) :: without_name)

let run_rescript_build ~root =
  let executable = local_platform_bin "rescript" in
  let env =
    Unix.environment ()
    |> with_env_var "RESCRIPT_RUNTIME"
         (source_root () // "packages" // "@rescript" // "runtime")
  in
  let null_device = if Sys.os_type = "Win32" then "NUL" else "/dev/null" in
  let dev_null = Unix.openfile null_device [Unix.O_WRONLY] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close dev_null)
    (fun () ->
      with_chdir root (fun () ->
          let pid =
            Unix.create_process_env executable [|executable; "build"|] env
              Unix.stdin dev_null dev_null
          in
          match snd (Unix.waitpid [] pid) with
          | Unix.WEXITED 0 -> ()
          | Unix.WEXITED code ->
            failwith (Printf.sprintf "rescript build exited with code %d" code)
          | Unix.WSIGNALED signal ->
            failwith
              (Printf.sprintf "rescript build killed by signal %d" signal)
          | Unix.WSTOPPED signal ->
            failwith
              (Printf.sprintf "rescript build stopped by signal %d" signal)))

module Client = struct
  (** Helpers for spawning the ReScript language server in tests, sending
  LSP requests/notifications over stdio, and reading responses back. *)

  type t = {
    process_stdout: in_channel;
    process_stdin: out_channel;
    mutable next_id: int;
    mutable stopped: bool;
  }

  let frame (json : Yojson.Safe.t) : string =
    let body = Yojson.Safe.to_string json in
    Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body

  let read_headers t =
    let rec loop acc =
      match input_line t.process_stdout with
      | line when String.trim line = "" -> Some acc
      | line ->
        let acc =
          match String.index_opt line ':' with
          | None -> acc
          | Some i ->
            let k = String.sub line 0 i in
            let v =
              String.trim (String.sub line (i + 1) (String.length line - i - 1))
            in
            (k, v) :: acc
        in
        loop acc
      | exception End_of_file -> if acc = [] then None else Some acc
    in
    loop []

  let read_message t =
    match read_headers t with
    | None -> None
    | Some headers -> (
      let len = int_of_string (List.assoc "Content-Length" headers) in
      match really_input_string t.process_stdout len with
      | body -> Some (Yojson.Safe.from_string body)
      | exception End_of_file -> None)

  let start () =
    let process_stdout, process_stdin =
      Unix.open_process_args executable [|executable|]
    in
    set_binary_mode_in process_stdout true;
    set_binary_mode_out process_stdin true;
    {process_stdout; process_stdin; next_id = 0; stopped = false}

  let send_packet t (packet : Jsonrpc.Packet.t) =
    let json = Jsonrpc.Packet.yojson_of_t packet in
    output_string t.process_stdin (frame json);
    flush t.process_stdin

  let next_id t =
    t.next_id <- t.next_id + 1;
    t.next_id

  (** Send a typed LSP request and return the assigned id. *)
  let send_request t (req : 'r Lsp.Client_request.t) =
    let id = `Int (next_id t) in
    let jsonrpc_req = Lsp.Client_request.to_jsonrpc_request req ~id in
    send_packet t (Jsonrpc.Packet.Request jsonrpc_req);
    id

  (** Send a typed LSP notification. *)
  let send_notification t (notif : Lsp.Client_notification.t) =
    let jsonrpc_notif = Lsp.Client_notification.to_jsonrpc notif in
    send_packet t (Jsonrpc.Packet.Notification jsonrpc_notif)

  (** Read packets until we find the response matching [id]. Server
      notifications/requests received in the meantime are discarded. *)
  let rec read_response t id =
    match read_message t with
    | None -> failwith "Helper.read_response: unexpected EOF"
    | Some json -> (
      match Jsonrpc.Packet.t_of_yojson json with
      | Response resp when resp.id = id -> resp
      | _ -> read_response t id)

  (** Send a typed request and synchronously wait for its response, decoded
      back into the request's result type. *)
  let request (type r) t (req : r Lsp.Client_request.t) : r =
    let id = send_request t req in
    let resp = read_response t id in
    match resp.result with
    | Ok json -> Lsp.Client_request.response_of_json req json
    | Error err -> failwith ("LSP error response: " ^ err.message)

  let rec read_request t method_ =
    match read_message t with
    | None -> failwith "Helper.read_request: unexpected EOF"
    | Some json -> (
      match Jsonrpc.Packet.t_of_yojson json with
      | Request request when request.method_ = method_ -> request
      | _ -> read_request t method_)

  let respond_ok t (request : Jsonrpc.Request.t) =
    Jsonrpc.Response.ok request.id `Null |> fun response ->
    send_packet t (Jsonrpc.Packet.Response response)

  (** Read the next packet of any kind. Useful when waiting for a server
      notification (e.g. publishDiagnostics). *)
  (* let read_packet t =
    match read_message t with
    | None -> failwith "Helper.read_packet: unexpected EOF"
    | Some json -> Jsonrpc.Packet.t_of_yojson json *)

  let stop t =
    if t.stopped then Unix.WEXITED 0
    else (
      t.stopped <- true;
      (try close_out_noerr t.process_stdin with _ -> ());
      try Unix.close_process (t.process_stdout, t.process_stdin)
      with _ -> Unix.WEXITED 0)

  (** Run [f] with a freshly started server, ensuring the process is stopped
      and the switch is released afterwards. *)
  let with_server f =
    let t = start () in
    Fun.protect ~finally:(fun () -> ignore (stop t)) (fun () -> f t)
end

open Lsp
open Types

let make_document ~uri ~text =
  TextDocumentItem.create ~uri ~languageId:"rescript" ~version:0 ~text

let write_document path (doc : TextDocumentItem.t) =
  let path = path // (doc.uri |> Uri.to_path |> Filename.basename) in
  write_file path doc.text

let setup_project ~root ~rescript_json ~run_build ~documents =
  mkdir_p root;
  write_document root
    (TextDocumentItem.create ~languageId:"json" ~text:rescript_json
       ~uri:(Uri.of_string "rescript.json")
       ~version:0);
  List.iter (write_document root) documents;
  if run_build then run_rescript_build ~root

let client_capabilities =
  ClientCapabilities.create
    ~textDocument:
      (TextDocumentClientCapabilities.create
         ~diagnostic:(DiagnosticClientCapabilities.create ())
         ())
    ~workspace:
      (WorkspaceClientCapabilities.create
         ~diagnostics:
           (DiagnosticWorkspaceClientCapabilities.create ~refreshSupport:true ())
         ())
    ()

let initialize_client ~root client =
  let id =
    Client.send_request client
      (Client_request.Initialize
         (InitializeParams.create ~capabilities:client_capabilities
            ~rootUri:(DocumentUri.of_path root) ()))
  in
  assert ((Client.read_response client id).result |> Result.is_ok);
  Client.send_notification client Client_notification.Initialized;
  let registration = Client.read_request client "client/registerCapability" in
  Client.respond_ok client registration

type action =
  | Request : 'result Client_request.t * ('result -> unit) -> action
  | Notification of Client_notification.t

let perform client = function
  | Request (request, on_response) ->
    Client.request client request |> on_response
  | Notification notification -> Client.send_notification client notification

let perform_all client actions = List.iter (perform client) actions

let start_client ~root =
  let client = Client.start () in
  try
    initialize_client ~root client;
    client
  with exn ->
    ignore (Client.stop client);
    raise exn

let with_client ~root f =
  let client = start_client ~root in
  Fun.protect
    ~finally:(fun () -> ignore (Client.stop client))
    (fun () -> f client)

let open_document (doc : TextDocumentItem.t) =
  let notification =
    Client_notification.TextDocumentDidOpen
      (DidOpenTextDocumentParams.create
         ~textDocument:
           (TextDocumentItem.create ~uri:doc.uri ~languageId:"rescript"
              ~version:0 ~text:doc.text))
  in
  Notification notification

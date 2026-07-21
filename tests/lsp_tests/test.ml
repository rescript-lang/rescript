let ( // ) = Filename.concat
let ( / ) = Eio.Path.( / )
let executable = "_build" // "default" // "lsp" // "bin" // "main.exe"

module Client = struct
  (** Helpers for spawning the ReScript language server in tests, sending
      LSP requests/notifications over stdio, and reading responses back. *)

  type t = {
    proc: [`Generic | `Unix] Eio.Process.ty Eio.Resource.t;
    stdin: Eio_unix.sink_ty Eio.Resource.t;
    stdout: Eio.Buf_read.t;
    mutable next_id: int;
  }

  let frame (json : Yojson.Safe.t) : string =
    let body = Yojson.Safe.to_string json in
    Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body

  let read_headers buf =
    let rec loop acc =
      match Eio.Buf_read.line buf with
      | "" -> Some acc
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

  let read_message buf =
    match read_headers buf with
    | None -> None
    | Some headers ->
      let len = int_of_string (List.assoc "Content-Length" headers) in
      let body = Eio.Buf_read.take len buf in
      Some (Yojson.Safe.from_string body)

  let start ~sw ~env =
    let mgr = Eio.Stdenv.process_mgr env in
    let stdin_r, stdin_w = Eio_unix.pipe sw in
    let stdout_r, stdout_w = Eio_unix.pipe sw in
    let proc =
      Eio.Process.spawn ~sw mgr ~stdin:stdin_r ~stdout:stdout_w ~executable
        [executable; "--stdio"]
    in
    Eio.Resource.close stdin_r;
    Eio.Resource.close stdout_w;
    let stdout = Eio.Buf_read.of_flow ~max_size:(16 * 1024 * 1024) stdout_r in
    {proc; stdin = stdin_w; stdout; next_id = 0}

  let send_packet t (packet : Jsonrpc.Packet.t) =
    let json = Jsonrpc.Packet.yojson_of_t packet in
    Eio.Flow.copy_string (frame json) t.stdin

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
    match read_message t.stdout with
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

  (** Read the next packet of any kind. Useful when waiting for a server
      notification (e.g. publishDiagnostics). *)
  (* let read_packet t =
    match read_message t.stdout with
    | None -> failwith "Helper.read_packet: unexpected EOF"
    | Some json -> Jsonrpc.Packet.t_of_yojson json *)

  let stop t =
    (try Eio.Resource.close t.stdin with _ -> ());
    Eio.Process.await t.proc

  (** Run [f] with a freshly started server, ensuring the process is stopped
      and the switch is released afterwards. *)
  let with_server ~env f =
    Eio.Switch.run @@ fun sw ->
    let t = start ~sw ~env in
    Fun.protect ~finally:(fun () -> ignore (stop t)) (fun () -> f t)
end

open Lsp
open Types

type caret_comment = {
  path: string; (* absolute path *)
  line: int; (* line of the comment *)
  col: int; (* column of the ^ character *)
  command: string; (* e.g. "hov" *)
  text: string; (* file content *)
}

module String_map = Map.Make (String)

let find_caret_comments ~fs ~workspace_dir =
  let results = ref [] in

  (* Read all .res files in directory *)
  Eio.Path.with_open_dir
    Eio.Path.(fs / workspace_dir)
    (fun dir_handle ->
      Eio.Path.read_dir dir_handle
      |> List.filter (fun file ->
             String.ends_with ~suffix:".res" file
             || String.ends_with ~suffix:".resi" file)
      |> List.iter (fun filename ->
             let path = Eio.Path.(dir_handle / filename) in
             let content = Eio.Path.load path in
             let lines = String.split_on_char '\n' content in

             List.iteri
               (fun line_idx line ->
                 (* Match lines like "// ^command" *)
                 match String.trim line with
                 | s when String.length s > 3 && String.sub s 0 3 = "// " -> (
                   let rest = String.sub s 3 (String.length s - 3) in
                   match String.index_opt rest '^' with
                   | None -> ()
                   | Some caret_in_rest ->
                     (* Column of ^ in original line *)
                     let prefix_len =
                       String.length line - String.length (String.trim line)
                     in
                     let col = prefix_len + 3 + caret_in_rest in
                     let command =
                       let after = caret_in_rest + 1 in
                       if after < String.length rest then
                         String.trim
                           (String.sub rest after (String.length rest - after))
                       else ""
                     in
                     results :=
                       {
                         path = workspace_dir // snd path;
                         line = line_idx;
                         col;
                         command;
                         text = content;
                       }
                       :: !results)
                 | _ -> ())
               lines));

  List.rev !results

let open_document ~uri ~text client =
  Client.send_notification client
    (Client_notification.TextDocumentDidOpen
       (DidOpenTextDocumentParams.create
          ~textDocument:
            (TextDocumentItem.create ~uri ~languageId:"rescript" ~version:0
               ~text)))

let pretty_source_loc caret_comment =
  let relative_path =
    let dir_len = String.length (Sys.getcwd () ^ "/") in
    String.sub caret_comment.path dir_len
      (String.length caret_comment.path - dir_len)
  in

  Printf.sprintf "%s:%d:%d" relative_path caret_comment.line
    (caret_comment.col + 1)

let send_request payload client =
  let response = Client.request client payload in
  Client_request.yojson_of_result payload response
  |> Yojson.Safe.pretty_to_string ~std:true

let print_response method_ response caret_comment =
  Printf.sprintf "Request %s %s\nResponse\n%s\n\n" method_
    (pretty_source_loc caret_comment)
    response

let run_test_for_comment (caret_comment : caret_comment) client =
  let uri = DocumentUri.of_path caret_comment.path in
  let textDocument = TextDocumentIdentifier.create ~uri in

  let character = caret_comment.col in
  let line = caret_comment.line - 1 in
  let position = Position.create ~line ~character in

  match caret_comment.command with
  | "hov" ->
    let resp =
      send_request
        (Client_request.TextDocumentHover
           (HoverParams.create ~textDocument ~position ()))
        client
    in
    print_response "textDocument/hover" resp caret_comment
  (* | "cmp" ->
    let context =
      CompletionContext.create ~triggerCharacter:">"
        ~triggerKind:CompletionTriggerKind.TriggerCharacter ()
    in
    send_request
      (Client_request.TextDocumentCompletion
         (CompletionParams.create ~textDocument ~position ~context ()))
      "textDocument/completion" caret_comment *)
  | other ->
    Printf.sprintf "Command `%s` not implemented! %s\n\n" other
      (pretty_source_loc caret_comment)

let run_workspace_test ~fs ~workspace_dir client =
  let comments = find_caret_comments ~fs ~workspace_dir in

  let grouped =
    List.fold_left
      (fun acc comment ->
        let others =
          Option.value ~default:[] (String_map.find_opt comment.path acc)
        in
        String_map.add comment.path (comment :: others) acc)
      String_map.empty comments
  in

  String_map.iter
    (fun path comments ->
      let hd = comments |> List.hd in
      let uri = DocumentUri.of_path hd.path in
      let text = hd.text in

      open_document ~uri ~text client;

      let filename = Filename.basename path ^ ".expected" in
      let save_path = workspace_dir // filename in
      let content =
        List.rev_map (fun c -> run_test_for_comment c client) comments
        |> String.concat ""
      in
      let file = Eio.Path.(fs / save_path) in
      Eio.Path.save ~create:(`Or_truncate 0o644) file content)
    grouped

let client_capabilities = ClientCapabilities.create ()

let main () =
  let workspace_dir =
    Sys.getcwd () // "tests" // "lsp_tests" // "basic-workspace"
  in
  Eio_main.run @@ fun env ->
  Client.with_server ~env @@ fun client ->
  let id =
    Client.send_request client
      (Client_request.Initialize
         (InitializeParams.create ~capabilities:client_capabilities
            ~rootUri:(DocumentUri.of_path workspace_dir)
            ()))
  in
  (* Assert than server capabilities return is ok *)
  assert ((Client.read_response client id).result |> Result.is_ok);
  let () = Client.send_notification client Client_notification.Initialized in

  run_workspace_test ~fs:env#fs ~workspace_dir client;
  Client.stop client |> ignore

let () = main ()

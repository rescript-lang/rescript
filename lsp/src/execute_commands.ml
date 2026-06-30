open Lsp
open Types

let request_show_document ~uri ~takeFocus (server : State.t Server.t) =
  let state = Server.state server in
  let client_support_window_show_document =
    match (State.params state).capabilities.window with
    | Some {showDocument = Some {support}} -> support = true
    | _ -> false
  in
  if client_support_window_show_document then (
    Server.request
      (Server_request.ShowDocumentRequest
         (ShowDocumentParams.create ~takeFocus ~uri ()))
      server;
    Ok None)
  else
    (* TODO: Also show a window/showMessage notification when showDocument is
       unsupported. The current error response is easy to miss in clients. *)
    let message =
      match (State.params state).clientInfo with
      | Some {name; version} ->
        Printf.sprintf
          "The client %s (version %s) dont support window/showDocument request"
          name
          (Option.value version ~default:"unknown")
      | None -> "The client dont support window/showDocument request"
    in
    Error (Printf.sprintf "Failed to open %s. %s" (Uri.to_path uri) message)

module Open_compiled = struct
  let name = "rescript/openCompiled"
  let execute ~(arguments : Yojson.Safe.t list option)
      (server : State.t Server.t) =
    match arguments with
    | Some [`String uri] ->
      request_show_document ~uri:(Uri.of_string uri) ~takeFocus:true server
    | _ ->
      Error
        (Printf.sprintf
           "Invalid arguments for workspace/executeCommand %s. Expected a list \
            of string: [uri]"
           name)
end

module Create_interface = struct
  let name = "rescript/createInterface"
  let execute ~(arguments : Yojson.Safe.t list option)
      (server : State.t Server.t) =
    match arguments with
    | Some [`String uri; `String cmi_uri] -> (
      match
        Custom_requests.Create_interface_file.create ~uri:(Uri.of_string uri)
          ~cmi_uri:(Uri.of_string cmi_uri) ~state:(Server.state server)
      with
      | Ok uri -> request_show_document ~uri ~takeFocus:true server
      | Error _ ->
        (* TODO: Show an window/showMessage when interface creation fails
             because the project has not been built and the .cmi file is
             missing. *)
        Error
          (Printf.sprintf "Failed to create interface file for %s and %s" uri
             cmi_uri))
    | _ ->
      Error
        (Printf.sprintf
           "Invalid arguments for workspace/executeCommand %s. Expected a list \
            of string: [uri, cmi_uri]"
           name)
end

module Switch_implementation_interface = struct
  let name = "rescript/switchImplementationInterface"
  let execute ~(arguments : Yojson.Safe.t list option)
      (server : State.t Server.t) =
    match arguments with
    | Some [`String uri] ->
      request_show_document ~uri:(Uri.of_string uri) ~takeFocus:true server
    | _ ->
      Error
        (Printf.sprintf
           "Invalid arguments for workspace/executeCommand %s. Expected a list \
            of string: [uri]"
           name)
end

module Dump_server_state = struct
  let name = "rescript/dumpServerState"
  let execute ~(arguments : Yojson.Safe.t list option)
      (server : State.t Server.t) =
    let json =
      State.to_yojson (Server.state server) |> Yojson.Safe.pretty_to_string
    in
    let response = `Assoc [("content", `String json)] in
    Ok (Some response)
end [@warning "-27"]

module Dump_cmt = struct
  let name = "rescript/dumpCmt"
  let execute ~(arguments : Yojson.Safe.t list option)
      (server : State.t Server.t) =
    match arguments with
    | Some [`String uri] -> (
      let analysis_state = server |> Server.state |> State.analysis_state in
      match Helpers.load_full (Uri.of_string uri) analysis_state with
      | Some full ->
        let cmt_content =
          Analysis.Cmt_viewer.dump ~full ~filter_for_position:None
        in
        let response = `Assoc [("content", `String cmt_content)] in
        Ok (Some response)
      | None ->
        Error
          (Printf.sprintf
             "Failed to dump cmt for %s, we are unable to read the metadata of \
              cmt file"
             uri))
    | _ ->
      Error
        (Printf.sprintf
           "Invalid arguments for workspace/executeCommand %s. Expected a list \
            of string: [uri]"
           name)
end

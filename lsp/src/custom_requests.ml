open Lsp
open Lsp.Types

let make_error ?(code = Jsonrpc.Response.Error.Code.InternalError) message =
  Jsonrpc.Response.Error.make ~message ~code ()

module Open_compiled_file = struct
  let meth = "textDocument/openCompiled"

  (* Custom request to open compiled file

    Request params: TextDocumentIdentifier
    Return: TextDocumentIdentifier | ResponseError
  *)
  let on_request ~(params : Jsonrpc.Structured.t option) ~(state : State.t) =
    let make ~(uri : Uri.t) ~(state : State.t) =
      let compiled_uri =
        Helpers.get_compiled_file ~uri
          ~compiler_config:(State.compiler_config state)
          ~fs:state.fs
      in
      match compiled_uri with
      | Some uri -> Ok uri
      | None -> Error ("Failed to get compiled file for " ^ Uri.to_path uri)
    in
    match params with
    | Some json -> (
      let doc =
        json |> Jsonrpc.Structured.yojson_of_t
        |> TextDocumentIdentifier.t_of_yojson
      in
      match make ~uri:doc.uri ~state with
      | Ok uri ->
        Ok
          (TextDocumentIdentifier.create ~uri
          |> TextDocumentIdentifier.yojson_of_t)
      | Error message -> Error (make_error message))
    | _ ->
      Error
        (make_error ?code:(Some InvalidParams)
           (Printf.sprintf
              "Invalid params for request %s, expected a TextDocumentIdentifier"
              meth))
end

module Create_interface_file = struct
  let meth = "textDocument/createInterface"

  let make ~uri ~cmi_uri ~(state : State.t) =
    match Document.kind_of_uri uri with
    | Res -> (
      let resi_file = Uri.to_path uri ^ "i" in
      let already_exists () =
        Error
          (Printf.sprintf "Cannot create interface file %s: it already exists."
             resi_file)
      in
      if Fs.exists ~fs:state.fs ~follow:false resi_file then already_exists ()
      else
        match
          Analysis.Create_interface.command ~path:(Uri.to_path uri)
            ~source:(Document_store.get ~uri state.store |> Document.text)
            ~cmi_file:(cmi_uri |> Uri.to_path)
        with
        | Ok content -> (
          try
            (* A file may appear after the availability check, including after
               a code action was offered. Never truncate an existing interface. *)
            Fs.save ~append:false ~create:(`Exclusive 0o644) ~fs:state.fs
              resi_file content;
            Ok (Uri.of_path resi_file)
          with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) ->
            already_exists ())
        | Error e -> Error e)
    | _ ->
      Error
        (Printf.sprintf
           "Invalid file to create interface for %s. Expected a .res file"
           (Uri.to_string uri))

  (* Custom request to create a interface file.

    Request params: TextDocumentIdentifier
    Return: TextDocumentIdentifier | ResponseError
  *)
  let on_request ~(params : Jsonrpc.Structured.t option) ~(state : State.t) =
    match params with
    | Some json ->
      let doc =
        json |> Jsonrpc.Structured.yojson_of_t
        |> TextDocumentIdentifier.t_of_yojson
      in
      let uri = doc.uri in
      let cmi_file =
        Helpers.get_cmi_file ~uri ~fs:state.fs
          ~compiler_config:(State.compiler_config state)
      in
      let result =
        match cmi_file with
        | Some cmi_file ->
          let response =
            match make ~uri ~cmi_uri:(Uri.of_path cmi_file) ~state with
            | Ok uri ->
              Ok
                (TextDocumentIdentifier.create ~uri
                |> TextDocumentIdentifier.yojson_of_t)
            | Error message -> Error (make_error message)
          in
          response
        | None ->
          Error
            (make_error
               (Printf.sprintf
                  "Failed to find cmi file for %s to create interface file"
                  (Uri.to_string uri)))
      in
      result
    | _ ->
      Error
        (make_error ?code:(Some InvalidParams)
           (Printf.sprintf
              "Invalid params for request %s, expected a TextDocumentIdentifier"
              meth))
end

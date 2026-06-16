open Lsp

let make_error ?(code = Jsonrpc.Response.Error.Code.InternalError) message =
  Jsonrpc.Response.Error.make ~message ~code ()

module Open_compiled_file = struct
  let meth = "textDocument/openCompiled"
  let create ~(uri : Uri.t) ~(state : State.t) =
    let compiled_uri =
      Helpers.get_compiled_file ~uri
        ~compiler_config:(State.compiler_config state)
        ~fs:state.fs
        ~workspace_root:(State.workspace_root state)
    in
    match compiled_uri with
    | Some uri -> Ok uri
    | None -> Error ("Failed to get compiled file for " ^ Uri.to_path uri)

  (* Custom request to open compiled file

    Request params:
     {
        "uri": Uri.t
     }

     Response:
     {
        "uri": Uri.t
     }
  *)
  let on_request ~(params : Jsonrpc.Structured.t option) ~(state : State.t) =
    let r =
      match params with
      | Some (`Assoc fields) -> (
        match List.assoc_opt "uri" fields with
        | Some (`String uri) -> (
          let uri = Uri.of_string uri in
          match create ~uri ~state with
          | Ok uri -> Ok (`Assoc [("uri", `String (uri |> Uri.to_string))])
          | Error message ->
            Error (make_error ?code:(Some InternalError) message))
        | _ ->
          Error
            (make_error ?code:(Some InvalidParams)
               "Invalid params for request textDocument/createInterfaceFile"))
      | _ ->
        Error
          (make_error ?code:(Some InvalidParams)
             "Invalid params for request textDocument/createInterfaceFile")
    in
    r
end

module Create_interface_file = struct
  let meth = "textDocument/createInterface"

  let create ~uri ~cmi_uri ~(state : State.t) =
    match Document.kind uri with
    | Res -> (
      match
        Analysis.Create_interface.command
          ~source:(Document_store.get ~uri state.store).text
          ~cmi_file:(cmi_uri |> Uri.to_path)
      with
      | Ok content ->
        let resi_file = Uri.to_path uri ^ "i" in
        let () =
          Fs.save ~append:false ~create:(`Or_truncate 0o644) ~fs:state.fs
            resi_file content
        in
        Ok (Uri.of_path resi_file)
      | Error e -> Error e)
    | _ ->
      Error
        (Printf.sprintf
           "Invalid file to create interface for %s. Expected a .res file"
           (Uri.to_string uri))

  (* Custom request to create a interface file.

    Request params:
     {
        "uri": Uri.t
     }

     Response:
     {
        "uri": Uri.t
     }
  *)
  let on_request ~(params : Jsonrpc.Structured.t option) ~(state : State.t) =
    let r =
      match params with
      | Some (`Assoc fields) -> (
        match List.assoc_opt "uri" fields with
        | Some (`String uri) ->
          let uri = Uri.of_string uri in
          let cmi_file =
            Helpers.get_cmi_file ~uri ~fs:state.fs
              ~compiler_config:(State.compiler_config state)
              ~workspace_root:(State.workspace_root state)
          in
          let result =
            match cmi_file with
            | Some cmi_file ->
              let response =
                match create ~uri ~cmi_uri:(Uri.of_path cmi_file) ~state with
                | Ok uri ->
                  Ok (`Assoc [("uri", `String (uri |> Uri.to_string))])
                | Error message ->
                  Error (make_error ?code:(Some InternalError) message)
              in
              response
            | None ->
              Error
                (make_error ?code:(Some InternalError)
                   "Failed to find cmi file to create interface file")
          in
          result
        | _ ->
          Error
            (make_error ?code:(Some InvalidParams)
               "Invalid params for request textDocument/createInterfaceFile"))
      | _ ->
        Error
          (make_error ?code:(Some InvalidParams)
             "Invalid params for request textDocument/createInterfaceFile")
    in
    r
end

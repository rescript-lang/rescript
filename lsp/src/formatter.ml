open Lsp
open Types

let run ~uri ~(state : State.t) =
  let source = (Document_store.get ~uri state.store).text in
  let kind_file = Document.kind uri in

  let format ~source =
    let full_document_text_edit text =
      (* TODO: \n works on Windows? *)
      let lines = String.split_on_char '\n' text in
      let end_line, end_character =
        match List.rev lines with
        | [] -> (0, 0)
        | last_line :: rest ->
          (List.length rest - 1, String.length last_line - 1)
      in
      let range =
        Range.create
          ~start:(Position.create ~line:0 ~character:0)
          ~end_:(Position.create ~line:end_line ~character:end_character)
      in
      [TextEdit.create ~range ~newText:text]
    in

    let read_all_from_channel channel =
      let buffer = Buffer.create 4096 in
      let bytes = Bytes.create 4096 in
      let rec loop () =
        match input channel bytes 0 (Bytes.length bytes) with
        | 0 -> Buffer.contents buffer
        | read ->
          Buffer.add_subbytes buffer bytes 0 read;
          loop ()
      in
      loop ()
    in

    let process_status_to_string = function
      | Unix.WEXITED code -> Printf.sprintf "exited with code %d" code
      | Unix.WSIGNALED signal -> Printf.sprintf "killed by signal %d" signal
      | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal
    in

    (* TODO: Move this blocking process call into an Eio-friendly abstraction.
       Unix.open_process_args blocks the current domain; a system thread or a
       portable process helper would avoid stalling the server during format.
       We can se Run with Eio_unix.run_in_systhread? *)
    let executable =
      let executable_name =
        (* TODO: Confirm the Windows shim name across package managers. npm
           usually creates rescript.cmd, but pnpm/yarn shims may differ. *)
        if Sys.win32 then "rescript.cmd" else "rescript"
      in
      let root_path = State.workspace_root state |> Uri.to_path in
      let ( /+ ) = Filename.concat in
      (* TODO: Resolve the formatter executable with package-manager-aware
         lookup and return a clear error when the binary is missing. *)
      root_path /+ "node_modules" /+ ".bin" /+ executable_name
    in
    let extension_name = Document.to_string kind_file in
    let stdin, stdout =
      Unix.open_process_args executable
        [|executable; "format"; "--stdin"; "." ^ extension_name|]
    in
    let close_process_noerr () =
      try ignore (Unix.close_process (stdin, stdout)) with _ -> ()
    in
    try
      output_string stdout source;
      close_out stdout;
      let formatted = read_all_from_channel stdin in
      match Unix.close_process (stdin, stdout) with
      | Unix.WEXITED 0 -> Ok (full_document_text_edit formatted)
      | status ->
        Error
          (Printf.sprintf "%s %s" executable (process_status_to_string status))
    with exn ->
      close_out_noerr stdout;
      close_in_noerr stdin;
      close_process_noerr ();
      Error (Printexc.to_string exn)
  in

  let document_has_syntax_error =
    let engine = Res_driver.parsing_engine in
    match kind_file with
    | Res ->
      (engine.parse_implementation_from_source ~for_printer:false ~source)
        .invalid
    | Resi ->
      (engine.parse_interface_from_source ~for_printer:false ~source).invalid
    | _ -> assert false
  in

  match document_has_syntax_error with
  | false -> format ~source
  | true ->
    (* TODO: Decide the client-facing behavior for formatting invalid syntax.
       Returning null is spec-valid but silent; a response error or
       window/showMessage may be easier to understand. *)
    Error "Failed to format, document has syntax error"

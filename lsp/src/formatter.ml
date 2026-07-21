open Lsp
open Types

let run ~uri ~(state : State.t) =
  let source = (Document_store.get ~uri state.store).text in
  let kind_file = Document.kind uri in

  let full_document_text_edit text =
    (* TODO: \n works on Windows? *)
    let lines = String.split_on_char '\n' text in
    let end_line, end_character =
      match List.rev lines with
      | [] -> (0, 0)
      | last_line :: rest -> (List.length rest, String.length last_line - 1)
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

  let executable =
    let executable_name = "rescript" in
    let root_path = State.workspace_root state |> Uri.to_path in
    let ( /+ ) = Filename.concat in
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
  | false -> (
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
      Error (Printexc.to_string exn))
  | true ->
    Error
      (Printf.sprintf "Failed to format %s, document has syntax error"
         (Uri.to_path uri))

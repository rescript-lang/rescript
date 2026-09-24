type streams = {
  stdout: out_channel;
  stderr: out_channel;
  stdout_formatter: Format.formatter;
  stderr_formatter: Format.formatter;
}

let key = Domain.DLS.new_key (fun () -> None)
let current () = Domain.DLS.get key
let is_active () = Option.is_some (current ())

let stdout_channel () =
  match current () with
  | Some streams -> streams.stdout
  | None -> Stdlib.stdout

let stderr_channel () =
  match current () with
  | Some streams -> streams.stderr
  | None -> Stdlib.stderr

let stdout_formatter () =
  match current () with
  | Some streams -> streams.stdout_formatter
  | None -> Format.std_formatter

let stderr_formatter () =
  match current () with
  | Some streams -> streams.stderr_formatter
  | None -> Format.err_formatter

let write_stdout text = output_string (stdout_channel ()) text
let write_stderr text = output_string (stderr_channel ()) text
let print_stdout text = write_stdout (text ^ "\n")
let print_stderr text = write_stderr (text ^ "\n")

let with_capture action =
  let stdout_path, stdout =
    Filename.open_temp_file ~mode:[Open_binary] "rescript-compiler-stdout-"
      ".log"
  in
  let stderr_path, stderr =
    try
      Filename.open_temp_file ~mode:[Open_binary] "rescript-compiler-stderr-"
        ".log"
    with exn ->
      close_out_noerr stdout;
      Sys.remove stdout_path;
      raise exn
  in
  let previous = current () in
  let streams =
    {
      stdout;
      stderr;
      stdout_formatter = Format.formatter_of_out_channel stdout;
      stderr_formatter = Format.formatter_of_out_channel stderr;
    }
  in
  Domain.DLS.set key (Some streams);
  let remove path = try Sys.remove path with Sys_error _ -> () in
  let read path =
    let channel = open_in_bin path in
    Fun.protect
      (fun () -> really_input_string channel (in_channel_length channel))
      ~finally:(fun () -> close_in channel)
  in
  Fun.protect
    (fun () ->
      let result =
        Fun.protect action ~finally:(fun () ->
            Fun.protect
              (fun () ->
                Format.pp_print_flush streams.stdout_formatter ();
                Format.pp_print_flush streams.stderr_formatter ();
                flush stdout;
                flush stderr)
              ~finally:(fun () ->
                close_out_noerr stdout;
                close_out_noerr stderr;
                Domain.DLS.set key previous))
      in
      (result, read stdout_path, read stderr_path))
    ~finally:(fun () ->
      remove stdout_path;
      remove stderr_path)

type target = {buffer: Buffer.t; mutable file: (string * out_channel) option}

type streams = {
  stdout: target;
  stderr: target;
  stdout_formatter: Format.formatter;
  stderr_formatter: Format.formatter;
}

let key = Domain.DLS.new_key (fun () -> None)
let current () = Domain.DLS.get key
let is_active () = Option.is_some (current ())

let create_target () = {buffer = Buffer.create 128; file = None}

let write_substring target text offset length =
  match target.file with
  | None -> Buffer.add_substring target.buffer text offset length
  | Some (_, channel) -> output_substring channel text offset length

let write target text = write_substring target text 0 (String.length text)

let flush target =
  match target.file with
  | None -> ()
  | Some (_, channel) -> flush channel

let make_formatter target =
  Format.make_formatter (write_substring target) (fun () -> flush target)

(* Most requests only emit text through the formatter. An out_channel is
   needed for binary AST output and the few channel-based printers, so create
   its temporary file only when a caller asks for one. *)
let channel target formatter =
  Format.pp_print_flush formatter ();
  match target.file with
  | Some (_, channel) -> channel
  | None ->
    let path, channel =
      Filename.open_temp_file ~mode:[Open_binary] "rescript-compiler-output-"
        ".log"
    in
    (try output_string channel (Buffer.contents target.buffer)
     with exn ->
       close_out_noerr channel;
       Sys.remove path;
       raise exn);
    Buffer.clear target.buffer;
    target.file <- Some (path, channel);
    channel

let stdout_channel () =
  match current () with
  | Some streams -> channel streams.stdout streams.stdout_formatter
  | None -> Stdlib.stdout

let stderr_channel () =
  match current () with
  | Some streams -> channel streams.stderr streams.stderr_formatter
  | None -> Stdlib.stderr

let stdout_formatter () =
  match current () with
  | Some streams -> streams.stdout_formatter
  | None -> Format.std_formatter

let stderr_formatter () =
  match current () with
  | Some streams -> streams.stderr_formatter
  | None -> Format.err_formatter

let write_stdout text =
  match current () with
  | Some streams -> write streams.stdout text
  | None -> output_string Stdlib.stdout text

let write_stderr text =
  match current () with
  | Some streams -> write streams.stderr text
  | None -> output_string Stdlib.stderr text

let print_stdout text =
  write_stdout text;
  write_stdout "\n"

let print_stderr text =
  write_stderr text;
  write_stderr "\n"

let cleanup target =
  match target.file with
  | None -> ()
  | Some (path, channel) -> (
    target.file <- None;
    close_out_noerr channel;
    try Sys.remove path with Sys_error _ -> ())

let contents target =
  match target.file with
  | None -> Buffer.contents target.buffer
  | Some (path, channel) ->
    Stdlib.flush channel;
    close_out channel;
    target.file <- None;
    Fun.protect
      (fun () ->
        let input = open_in_bin path in
        Fun.protect
          (fun () -> really_input_string input (in_channel_length input))
          ~finally:(fun () -> close_in input))
      ~finally:(fun () -> Sys.remove path)

let with_capture action =
  let stdout = create_target () in
  let stderr = create_target () in
  let streams =
    {
      stdout;
      stderr;
      stdout_formatter = make_formatter stdout;
      stderr_formatter = make_formatter stderr;
    }
  in
  let previous = current () in
  Domain.DLS.set key (Some streams);
  Fun.protect
    (fun () ->
      let result = action () in
      Format.pp_print_flush streams.stdout_formatter ();
      Format.pp_print_flush streams.stderr_formatter ();
      (result, contents stdout, contents stderr))
    ~finally:(fun () ->
      Domain.DLS.set key previous;
      cleanup stdout;
      cleanup stderr)

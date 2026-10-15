(* One captured stream. Output collects in memory; a temporary file is created
   only when a caller needs a real channel, which only compiler modes that
   write a binary AST or JavaScript to stdout do. *)
type sink = {
  prefix: string;
  buffer: Buffer.t;
  mutable file: (string * out_channel) option;
}

type streams = {
  stdout: sink;
  stderr: sink;
  stdout_formatter: Format.formatter;
  stderr_formatter: Format.formatter;
}

let key = Domain.DLS.new_key (fun () -> None)
let current () = Domain.DLS.get key
let is_active () = Option.is_some (current ())

(* A text-mode channel, like a bsc process's stdout and stderr, writes CRLF
   line endings on Windows. A line that already ends in CRLF, such as a PPX's
   output, keeps it. *)
let text_mode_line_endings ~win32 text =
  if not win32 then text
  else
    let converted = Buffer.create (String.length text) in
    String.iteri
      (fun index character ->
        if character = '\n' && (index = 0 || text.[index - 1] <> '\r') then
          Buffer.add_char converted '\r';
        Buffer.add_char converted character)
      text;
    Buffer.contents converted

(* The inverse, for text written to a text-mode channel, which adds the CR
   itself: without it, a line that already ends in CRLF would get CR CR LF. *)
let text_mode_input ~win32 text =
  if (not win32) || not (String.contains text '\r') then text
  else
    let converted = Buffer.create (String.length text) in
    String.iteri
      (fun index character ->
        if
          not
            (character = '\r'
            && index + 1 < String.length text
            && text.[index + 1] = '\n')
        then Buffer.add_char converted character)
      text;
    Buffer.contents converted

let write_sink sink text position length =
  match sink.file with
  | Some (_, channel) ->
    output_string channel
      (text_mode_input ~win32:Sys.win32 (String.sub text position length))
  | None -> Buffer.add_substring sink.buffer text position length

let sink_channel sink =
  match sink.file with
  | Some (_, channel) -> channel
  | None ->
    let path, channel =
      Filename.open_temp_file ~mode:[Open_text] sink.prefix ".log"
    in
    (* Output written so far goes first. The channel is in text mode, so it
       already converts line endings. *)
    output_string channel
      (text_mode_input ~win32:Sys.win32 (Buffer.contents sink.buffer));
    Buffer.clear sink.buffer;
    sink.file <- Some (path, channel);
    channel

let sink_contents sink =
  match sink.file with
  | None ->
    text_mode_line_endings ~win32:Sys.win32 (Buffer.contents sink.buffer)
  | Some (path, channel) ->
    close_out channel;
    let input = open_in_bin path in
    Fun.protect
      (fun () -> really_input_string input (in_channel_length input))
      ~finally:(fun () -> close_in_noerr input)

let close_sink sink =
  match sink.file with
  | None -> ()
  | Some (path, channel) ->
    close_out_noerr channel;
    (try Sys.remove path with Sys_error _ -> ());
    sink.file <- None

let sink_formatter sink =
  Format.make_formatter (write_sink sink) (fun () ->
      Option.iter (fun (_, channel) -> flush channel) sink.file)

let stdout_channel () =
  match current () with
  | Some streams -> sink_channel streams.stdout
  | None -> Stdlib.stdout

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
  | Some streams -> write_sink streams.stdout text 0 (String.length text)
  | None -> output_string Stdlib.stdout text

let write_stderr text =
  match current () with
  | Some streams -> write_sink streams.stderr text 0 (String.length text)
  | None -> output_string Stdlib.stderr text

(* Like print_endline and prerr_endline, a line printed outside a capture is
   flushed: a subprocess the compiler runs next, such as a PPX, writes to the
   same terminal. *)
let print_stdout text =
  write_stdout (text ^ "\n");
  if not (is_active ()) then flush Stdlib.stdout

let print_stderr text =
  write_stderr (text ^ "\n");
  if not (is_active ()) then flush Stdlib.stderr

exception Failed of {error: exn; stdout: string; stderr: string}

let with_capture action =
  let sink prefix = {prefix; buffer = Buffer.create 256; file = None} in
  let stdout = sink "rescript-compiler-stdout-" in
  let stderr = sink "rescript-compiler-stderr-" in
  let streams =
    {
      stdout;
      stderr;
      stdout_formatter = sink_formatter stdout;
      stderr_formatter = sink_formatter stderr;
    }
  in
  let previous = current () in
  Domain.DLS.set key (Some streams);
  let contents () =
    Format.pp_print_flush streams.stdout_formatter ();
    Format.pp_print_flush streams.stderr_formatter ();
    (sink_contents stdout, sink_contents stderr)
  in
  Fun.protect
    (fun () ->
      match action () with
      | result ->
        let stdout, stderr = contents () in
        (result, stdout, stderr)
      | exception error ->
        let backtrace = Printexc.get_raw_backtrace () in
        let stdout, stderr = contents () in
        Printexc.raise_with_backtrace (Failed {error; stdout; stderr}) backtrace)
    ~finally:(fun () ->
      close_sink stdout;
      close_sink stderr;
      Domain.DLS.set key previous)

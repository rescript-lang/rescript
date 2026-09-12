let path_separator = ';'
let terminal_supports_color_without_term = true

let configure_standard_streams () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  set_binary_mode_out stderr true
let clean_symbol = "[clean] "
let parse_symbol = "[parse] "
let build_symbol = "[build] "
let success_symbol = "[ok] "
let warning_symbol = "[warn] "
let error_symbol = "[error] "
let normalize_path_for_comparison path =
  path |> String.lowercase_ascii
  |> String.map (function '/' -> '\\' | character -> character)

let strip_verbatim_prefix path =
  if String.starts_with ~prefix:"\\\\?\\UNC\\" path then
    "\\\\" ^ String.sub path 8 (String.length path - 8)
  else if String.starts_with ~prefix:"\\\\?\\" path then
    String.sub path 4 (String.length path - 4)
  else path

let canonicalize_path path = Unix.realpath path |> strip_verbatim_prefix

external directory_file_identity : string -> string
  = "rewatch_windows_directory_identity"

let directory_identity ~path _metadata = directory_file_identity path

let executable_extensions ~program =
  if Filename.extension program <> "" then [""]
  else
    Sys.getenv_opt "PATHEXT"
    |> Option.value ~default:".COM;.EXE;.BAT;.CMD"
    |> String.split_on_char ';'

let search_directories ~cwd directories = cwd :: directories

let executable_is_usable candidate =
  try (Unix.stat candidate).Unix.st_kind = Unix.S_REG
  with Unix.Unix_error _ -> false

let normalize_path_directory directory =
  let directory = String.trim directory in
  let length = String.length directory in
  if length >= 2 && directory.[0] = '"' && directory.[length - 1] = '"' then
    String.sub directory 1 (length - 2)
  else directory

let resolve_program =
  Platform_common.resolve_program ~path_separator ~executable_extensions
    ~search_directories ~normalize_directory:normalize_path_directory
    ~executable_is_usable

type command = {env: Spawn.Env.t option; program: string; args: string list}

let post_build_command ~command ~output =
  let variable = "REWATCH_JS_POST_BUILD_FILE" in
  let prefix = String.lowercase_ascii (variable ^ "=") in
  let environment =
    Unix.environment () |> Array.to_list
    |> List.filter (fun entry ->
        not (String.starts_with ~prefix (String.lowercase_ascii entry)))
    |> List.cons (variable ^ "=" ^ output)
    |> Spawn.Env.of_list
  in
  {
    env = Some environment;
    program = "cmd.exe";
    args = ["/D"; "/V:OFF"; "/S"; "/C"; command ^ " \"%" ^ variable ^ "%\""];
  }

let is_batch_file program =
  List.mem
    (Filename.extension program |> String.lowercase_ascii)
    [".bat"; ".cmd"]

type process_job

type process = {wait_id: int; job: process_job}

external spawn_owned :
  env:Spawn.Env.t option ->
  cwd:string ->
  program:string ->
  command_line:string ->
  stdin:Unix.file_descr ->
  stdout:Unix.file_descr ->
  stderr:Unix.file_descr ->
  process = "rewatch_windows_spawn_owned_byte" "rewatch_windows_spawn_owned"

external terminate_process_job : process_job -> bool
  = "rewatch_windows_terminate_process_job"

external close_process_job : process_job -> unit
  = "rewatch_windows_close_process_job"

let quote_argument argument =
  if
    argument = ""
    || String.contains argument ' '
    || String.contains argument '\t'
    || String.contains argument '"'
  then Filename.quote argument
  else argument

let ensure_no_null label value =
  if String.contains value '\x00' then
    invalid_arg (Printf.sprintf "%s contains a NUL byte" label)

let program_for_working_directory ~cwd program =
  if Filename.is_relative program then Filename.concat cwd program else program

let serialize_command_line ~program ~args =
  match args with
  | ["/D"; "/V:OFF"; "/S"; "/C"; command] ->
    String.concat " " [quote_argument program; "/D"; "/V:OFF"; "/S"; "/C"]
    ^ " " ^ command
  | _ -> program :: args |> List.map quote_argument |> String.concat " "

let spawn ~env ~cwd ~program ~args ~stdin ~stdout ~stderr =
  let program = resolve_program ~cwd program in
  ensure_no_null "working directory" cwd;
  ensure_no_null "program" program;
  List.iter (ensure_no_null "argument") args;
  let program, args =
    if is_batch_file program then
      let command = Filename.quote_command program args in
      (resolve_program ~cwd "cmd.exe", ["/D"; "/V:OFF"; "/S"; "/C"; command])
    else (program, args)
  in
  let command_line = serialize_command_line ~program ~args in
  let program = program_for_working_directory ~cwd program in
  (* Starting suspended closes the only interval in which a child could create
     descendants before the job owns its process tree. *)
  spawn_owned ~env ~cwd ~program ~command_line ~stdin ~stdout ~stderr

let null_device = "NUL"

let process_id process = process.wait_id
let release_process process = close_process_job process.job

let create_capture_pipes = Platform_common.create_capture_pipes

let signal_process_tree ~root_reaped:_ process _signal =
  terminate_process_job process.job

let termination_signal_mutex = Mutex.create ()

let defer_termination_signals () =
  (* Windows has no per-thread signal mask, so temporary handlers affect every
     domain. Serializing installation keeps concurrently published artifacts
     from restoring one another's handlers out of order. *)
  Mutex.lock termination_signal_mutex;
  let pending = ref [] in
  let defer signal =
    if not (List.mem signal !pending) then pending := signal :: !pending
  in
  let previous_int =
    try Sys.signal Sys.sigint (Sys.Signal_handle defer)
    with exn ->
      Mutex.unlock termination_signal_mutex;
      raise exn
  in
  let previous_term =
    try Sys.signal Sys.sigterm (Sys.Signal_handle defer)
    with exn ->
      let exn =
        try
          ignore (Sys.signal Sys.sigint previous_int);
          exn
        with restore_exn -> restore_exn
      in
      Mutex.unlock termination_signal_mutex;
      raise exn
  in
  let restored = ref false in
  let dispatch signal behavior =
    match behavior with
    | Sys.Signal_ignore -> ()
    | Sys.Signal_handle handler -> handler signal
    | Sys.Signal_default -> raise Sys.Break
  in
  fun () ->
    if not !restored then (
      restored := true;
      let restore_error = ref None in
      let restore signal behavior =
        try ignore (Sys.signal signal behavior)
        with exn ->
          if Option.is_none !restore_error then restore_error := Some exn
      in
      restore Sys.sigint previous_int;
      restore Sys.sigterm previous_term;
      Mutex.unlock termination_signal_mutex;
      Option.iter raise !restore_error;
      List.rev !pending
      |> List.iter (fun signal ->
          dispatch signal
            (if signal = Sys.sigint then previous_int else previous_term)))

let graceful_termination_signal = Sys.sigkill
let escalate_process_groups = false

let parse_tasklist_csv_line line =
  let length = String.length line in
  let rec parse_field fields index =
    if index >= length || line.[index] <> '"' then None
    else
      let buffer = Buffer.create 32 in
      let rec parse_char index =
        if index >= length then None
        else
          match line.[index] with
          | '"' when index + 1 < length && line.[index + 1] = '"' ->
            Buffer.add_char buffer '"';
            parse_char (index + 2)
          | '"' ->
            let fields = Buffer.contents buffer :: fields in
            let next = index + 1 in
            if next = length then Some (List.rev fields)
            else if line.[next] = ',' then parse_field fields (next + 1)
            else None
          | character ->
            Buffer.add_char buffer character;
            parse_char (index + 1)
      in
      parse_char (index + 1)
  in
  if length = 0 then None else parse_field [] 0

type tasklist_probe = Process_found | Process_absent | Malformed_output

let tasklist_probe ~pid output =
  let lines =
    output |> String.trim |> String.split_on_char '\n' |> List.map String.trim
    |> List.filter (( <> ) "")
  in
  let rows = List.map parse_tasklist_csv_line lines in
  let valid_row = function
    | Some [_image; row_pid; _session; _session_number; _memory] ->
      Option.is_some (int_of_string_opt row_pid)
    | Some _ | None -> false
  in
  if lines = [] || not (List.for_all valid_row rows) then Malformed_output
  else if
    List.exists
      (function
        | Some [image; row_pid; _session; _session_number; _memory] ->
          String.starts_with ~prefix:"rescript" (String.lowercase_ascii image)
          && row_pid = string_of_int pid
        | Some _ | None -> false)
      rows
  then Process_found
  else Process_absent

let probe_process ~run pid =
  let tasklist =
    match Sys.getenv_opt "SystemRoot" with
    | Some root ->
      Filename.concat (Filename.concat root "System32") "tasklist.exe"
    | None -> "tasklist.exe"
  in
  match run tasklist ["/FO"; "CSV"; "/NH"] with
  | Some (Unix.WEXITED 0, stdout) -> (
    match tasklist_probe ~pid stdout with
    | Process_found | Malformed_output -> true
    | Process_absent -> false)
  | Some _ | None -> true

let process_is_active ~run value =
  Platform_common.process_is_active ~probe:(probe_process ~run) value

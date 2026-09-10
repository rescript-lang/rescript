let path_separator = ';'
let normalize_path_for_comparison = String.lowercase_ascii

let strip_verbatim_prefix path =
  if String.starts_with ~prefix:"\\\\?\\UNC\\" path then
    "\\\\" ^ String.sub path 8 (String.length path - 8)
  else if String.starts_with ~prefix:"\\\\?\\" path then
    String.sub path 4 (String.length path - 4)
  else path

let canonicalize_path path = Unix.realpath path |> strip_verbatim_prefix

let directory_identity ~path _metadata =
  canonicalize_path path |> normalize_path_for_comparison

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

let resolve_program =
  Platform_common.resolve_program ~path_separator ~executable_extensions
    ~search_directories ~executable_is_usable

let post_build_command ~command ~output =
  let variable = "REWATCH_JS_POST_BUILD_FILE" in
  let prefix = String.lowercase_ascii (variable ^ "=") in
  let environment =
    Unix.environment () |> Array.to_list
    |> List.filter (fun entry ->
         not
           (String.starts_with ~prefix (String.lowercase_ascii entry)))
    |> List.cons (variable ^ "=" ^ output)
    |> Spawn.Env.of_list
  in
  ( Some environment,
    "cmd.exe",
    ["/D"; "/V:OFF"; "/S"; "/C"; command ^ " \"%" ^ variable ^ "%\""] )

let is_batch_file program =
  List.mem
    (Filename.extension program |> String.lowercase_ascii)
    [".bat"; ".cmd"]

let spawn ~env ~cwd ~program ~args ~stdout ~stderr =
  let program = resolve_program ~cwd program in
  let program, args =
    if is_batch_file program then
      let command = Filename.quote_command program args in
      (resolve_program ~cwd "cmd.exe", ["/D"; "/V:OFF"; "/S"; "/C"; command])
    else (program, args)
  in
  Spawn.spawn ?env ~cwd:(Spawn.Working_dir.Path cwd) ~prog:program
    ~argv:(program :: args) ~stdout ~stderr ()

let create_capture_pipes () =
  let stdout = Spawn.safe_pipe () in
  try (stdout, Spawn.safe_pipe ())
  with exn ->
    Unix.close (fst stdout);
    Unix.close (snd stdout);
    raise exn

let signal_process_tree ~root_reaped pid _signal =
  (* A reaped Windows PID is no longer a safe process-tree identity because the
     operating system may reuse it for an unrelated process. Native Windows
     descendant cleanup therefore needs a retained process or job handle rather
     than another taskkill invocation. *)
  if root_reaped then ()
  else
  let taskkill =
    match Sys.getenv_opt "SystemRoot" with
    | Some root ->
      Filename.concat (Filename.concat root "System32") "taskkill.exe"
    | None -> "taskkill.exe"
  in
  let output = ref None in
  let killer_pid = ref None in
  let fallback () =
    try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()
  in
  try
    let null = Unix.openfile Filename.null [Unix.O_WRONLY] 0o600 in
    output := Some null;
    let killer =
      Spawn.spawn ~prog:taskkill
        ~argv:[taskkill; "/PID"; string_of_int pid; "/T"; "/F"]
        ~stdout:null ~stderr:null ()
    in
    killer_pid := Some killer;
    Unix.close null;
    output := None;
    let _, status = Unix.waitpid [] killer in
    killer_pid := None;
    if status <> Unix.WEXITED 0 then fallback ()
  with _ ->
    Option.iter
      (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
      !output;
    Option.iter
      (fun killer ->
        (try Unix.kill killer Sys.sigkill with Unix.Unix_error _ -> ());
        try ignore (Unix.waitpid [] killer) with Unix.Unix_error _ -> ())
      !killer_pid;
    fallback ()

let defer_termination_signals () =
  let pending = ref [] in
  let defer signal =
    if not (List.mem signal !pending) then pending := signal :: !pending
  in
  let previous_int = Sys.signal Sys.sigint (Sys.Signal_handle defer) in
  let previous_term =
    try Sys.signal Sys.sigterm (Sys.Signal_handle defer)
    with exn ->
      ignore (Sys.signal Sys.sigint previous_int);
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
      ignore (Sys.signal Sys.sigint previous_int);
      ignore (Sys.signal Sys.sigterm previous_term);
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

let tasklist_probe ~pid output =
  let lines =
    output |> String.trim |> String.split_on_char '\n'
    |> List.map String.trim |> List.filter (( <> ) "")
  in
  let rows = List.map parse_tasklist_csv_line lines in
  let valid_row = function
    | Some [_image; row_pid; _session; _session_number; _memory] ->
      Option.is_some (int_of_string_opt row_pid)
    | Some _ | None -> false
  in
  if lines = [] || not (List.for_all valid_row rows) then None
  else
    Some
      (List.exists
         (function
           | Some [image; row_pid; _session; _session_number; _memory] ->
             String.starts_with ~prefix:"rescript"
               (String.lowercase_ascii image)
             && row_pid = string_of_int pid
           | Some _ | None -> false)
         rows)

let tasklist_has_process ~pid output = tasklist_probe ~pid output = Some true

let probe_process ~run pid =
  let tasklist =
    match Sys.getenv_opt "SystemRoot" with
    | Some root ->
      Filename.concat (Filename.concat root "System32") "tasklist.exe"
    | None -> "tasklist.exe"
  in
  match run tasklist ["/FO"; "CSV"; "/NH"] with
  | Some (Unix.WEXITED 0, stdout) ->
    Option.value (tasklist_probe ~pid stdout) ~default:true
  | Some _ | None -> true

let process_is_active ~run value =
  Platform_common.process_is_active ~probe:(probe_process ~run) value

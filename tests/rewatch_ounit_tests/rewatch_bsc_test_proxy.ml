let getenv name =
  match Sys.getenv_opt name with
  | Some value -> value
  | None -> failwith ("Missing environment variable " ^ name)

let touch path = close_out (open_out_bin path)

let append_line path line =
  let channel = open_out_gen [Open_creat; Open_text; Open_append] 0o666 path in
  output_string channel line;
  output_char channel '\n';
  close_out channel

let remove_if_present path =
  try Sys.remove path with Sys_error _ -> ()

let has_argument expected =
  Array.exists (String.equal expected) Sys.argv

let output_argument () =
  let rec loop index =
    if index + 1 >= Array.length Sys.argv then None
    else if Sys.argv.(index) = "-o" then Some Sys.argv.(index + 1)
    else loop (index + 1)
  in
  loop 1

let basename path =
  path |> String.map (fun character -> if character = '\\' then '/' else character)
  |> Filename.basename

let arguments () =
  Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))

let wait_for_file path =
  let rec wait attempts =
    if attempts < 400 && not (Sys.file_exists path) then (
      Unix.sleepf 0.05;
      wait (attempts + 1))
  in
  wait 0

let run_compiler () =
  let program = getenv "REWATCH_REAL_BSC" in
  let argv = Array.copy Sys.argv in
  argv.(0) <- program;
  let pid = Unix.create_process program argv Unix.stdin Unix.stdout Unix.stderr in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED status -> status
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal

let block_for_scope_transition () =
  let request = getenv "REWATCH_SCOPE_BLOCK_REQUEST" in
  let started = getenv "REWATCH_SCOPE_BLOCK_STARTED" in
  if Sys.file_exists request && not (Sys.file_exists started) then (
    touch started;
    let release = getenv "REWATCH_SCOPE_BLOCK_RELEASE" in
    let rec wait attempts =
      if attempts < 200 && not (Sys.file_exists release) then (
        Unix.sleepf 0.05;
        wait (attempts + 1))
    in
    wait 0)

let before_compiler mode is_parse =
  if mode = "scope-block" then block_for_scope_transition ();
  if mode = "block-compile" && not is_parse then (
    touch (getenv "REWATCH_OCAML_COMPILE_STARTED");
    wait_for_file (getenv "REWATCH_OCAML_RELEASE_FILE"));
  if mode = "slow" then (
    touch (getenv "REWATCH_OCAML_CHILD_STARTED");
    match Sys.getenv_opt "REWATCH_OCAML_RELEASE_FILE" with
    | Some release -> wait_for_file release
    | None -> Unix.sleepf 5.0);
  if mode = "counting" || mode = "parse-warning-log" then
    append_line (getenv "REWATCH_BSC_CALL_LOG")
      (String.concat " " (arguments ()))

let after_compiler mode is_parse status =
  if status = 0 && mode = "parse-warning" && is_parse then
    prerr_endline "PARSE_WARNING_MARKER";
  if status = 0 && mode = "parse-warning-log" && is_parse then (
    let source = getenv "REWATCH_PARSE_WARNING_SOURCE" in
    if Array.exists (fun argument -> basename argument = source) Sys.argv then
      prerr_endline "REWATCH_PARSE_WARNING");
  if status = 0 && mode = "fail-late-publication" && not is_parse
     && Array.exists (fun argument -> basename argument = "A.ast") Sys.argv
     && Sys.file_exists (getenv "REWATCH_FAIL_PUBLICATION")
     && not (Sys.file_exists (getenv "REWATCH_PUBLICATION_FAILED"))
  then (
    let destination = getenv "REWATCH_PUBLICATION_DESTINATION" in
    remove_if_present destination;
    Unix.mkdir destination 0o755;
    touch (getenv "REWATCH_PUBLICATION_FAILED"))

let () =
  let mode = getenv "REWATCH_BSC_PROXY_MODE" in
  let is_parse = has_argument "-bs-ast" in
  before_compiler mode is_parse;
  if mode = "delete-parse-sources" && is_parse then (
    let marker = getenv "REWATCH_SOURCES_DELETED" in
    if not (Sys.file_exists marker) then (
      remove_if_present (getenv "REWATCH_SOURCE_A");
      remove_if_present (getenv "REWATCH_SOURCE_B");
      touch marker));
  let status = run_compiler () in
  after_compiler mode is_parse status;
  if status = 0 then
    if mode = "delete-source" && not is_parse then (
      let marker = getenv "REWATCH_SOURCE_DELETED" in
      if not (Sys.file_exists marker) then (
        remove_if_present (getenv "REWATCH_SOURCE_TO_DELETE");
        touch marker))
    else if mode = "delete-ast" && is_parse then
      match output_argument () with
      | None -> ()
      | Some output ->
        let marker = getenv "REWATCH_AST_DELETED" in
        if not (Sys.file_exists marker) then (
          remove_if_present output;
          touch marker);
  exit status

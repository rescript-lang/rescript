let getenv name =
  match Sys.getenv_opt name with
  | Some value -> value
  | None -> failwith ("Missing environment variable " ^ name)

let touch path = close_out (open_out_bin path)

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

let () =
  let mode = getenv "REWATCH_BSC_PROXY_MODE" in
  let is_parse = has_argument "-bs-ast" in
  if mode = "scope-block" then block_for_scope_transition ();
  if mode = "delete-parse-sources" && is_parse then (
    let marker = getenv "REWATCH_SOURCES_DELETED" in
    if not (Sys.file_exists marker) then (
      remove_if_present (getenv "REWATCH_SOURCE_A");
      remove_if_present (getenv "REWATCH_SOURCE_B");
      touch marker));
  let status = run_compiler () in
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

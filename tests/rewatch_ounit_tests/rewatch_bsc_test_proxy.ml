(* A bsc stand-in for Rust rewatch in the watch performance gate: it logs each
   call's arguments to REWATCH_BSC_CALL_LOG, then runs REWATCH_REAL_BSC. OCaml
   rewatch compiles in-process and logs its requests itself. *)

let getenv name =
  match Sys.getenv_opt name with
  | Some value -> value
  | None -> failwith ("Missing environment variable " ^ name)

(* Parallel compiler calls append to one log. Windows emulates append mode by
   seeking before each write, so concurrent appends can overwrite each other.
   A one-byte lock on a separate file serializes them on every platform;
   Windows rejects locking an unbounded range. *)
let append_line path line =
  let lock = Unix.openfile (path ^ ".lock") [Unix.O_RDWR; Unix.O_CREAT] 0o666 in
  Fun.protect
    ~finally:(fun () -> Unix.close lock)
    (fun () ->
      Unix.lockf lock Unix.F_LOCK 1;
      let channel =
        open_out_gen [Open_creat; Open_text; Open_append] 0o666 path
      in
      output_string channel line;
      output_char channel '\n';
      close_out channel)

let run_compiler () =
  let program = getenv "REWATCH_REAL_BSC" in
  let argv = Array.copy Sys.argv in
  argv.(0) <- program;
  let pid =
    Unix.create_process program argv Unix.stdin Unix.stdout Unix.stderr
  in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED status -> status
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal

let () =
  append_line
    (getenv "REWATCH_BSC_CALL_LOG")
    (String.concat " " (List.tl (Array.to_list Sys.argv)));
  exit (run_compiler ())

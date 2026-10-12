(* Shells report a process killed by signal n as exit status 128 + n. *)
let exit_code signal =
  if signal = Sys.sigint then 130
  else if signal = Sys.sigterm then 143
  else if signal = Sys.sighup then 129
  else if signal = Sys.sigquit then 131
  else 1

let received = Atomic.make 0

(* The first signal asks for a graceful stop, which waits for modules that are
   compiling in-process to finish. A second one exits at once: only the main
   domain publishes outputs, so an abandoned compile leaves nothing the next
   build would trust. *)
let exit_now signal =
  Build_lock.release_held ();
  (try
     flush stdout;
     flush stderr
   with Sys_error _ -> ());
  Unix._exit (exit_code signal)

let with_handlers handler action =
  let rec install = function
    | [] -> action ()
    | signal :: signals ->
      let handle signal =
        if Atomic.fetch_and_add received 1 = 0 then handler signal
        else exit_now signal
      in
      let previous = Sys.signal signal (Sys.Signal_handle handle) in
      Fun.protect
        (fun () -> install signals)
        ~finally:(fun () -> ignore (Sys.signal signal previous))
  in
  install Platform.termination_signals

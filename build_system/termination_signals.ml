(* Shells report a process killed by signal n as exit status 128 + n. *)
let exit_code signal =
  if signal = Sys.sigint then 130
  else if signal = Sys.sigterm then 143
  else if signal = Sys.sighup then 129
  else if signal = Sys.sigquit then 131
  else 1

let with_handlers handler action =
  let rec install = function
    | [] -> action ()
    | signal :: signals ->
      let previous = Sys.signal signal (Sys.Signal_handle handler) in
      Fun.protect
        (fun () -> install signals)
        ~finally:(fun () -> ignore (Sys.signal signal previous))
  in
  install Platform.termination_signals

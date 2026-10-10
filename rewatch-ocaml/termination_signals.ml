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

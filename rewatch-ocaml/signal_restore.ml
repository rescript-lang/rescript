type t = {restore_once: unit -> unit; mutable restored: bool}

let with_termination_handlers handler action =
  let rec install = function
    | [] -> action ()
    | signal :: signals ->
      let previous = Sys.signal signal (Sys.Signal_handle handler) in
      Fun.protect
        (fun () -> install signals)
        ~finally:(fun () -> ignore (Sys.signal signal previous))
  in
  install Platform.termination_signals

let create ~defer =
  {
    restore_once =
      (if defer then Platform.defer_termination_signals () else Fun.id);
    restored = false;
  }

let restore state =
  if not state.restored then (
    state.restored <- true;
    state.restore_once ())

let exception_after_restore state original =
  try
    restore state;
    original
  with restoration_error -> restoration_error

let protect state action =
  try
    let result = action () in
    restore state;
    result
  with error -> raise (exception_after_restore state error)

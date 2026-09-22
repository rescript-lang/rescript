type t = {restore_once: unit -> unit; mutable restored: bool}

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

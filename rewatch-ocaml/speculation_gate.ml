type state = Waiting | Open | Closed

type t = {mutex: Mutex.t; changed: Condition.t; mutable state: state}

exception Abandoned

let create () =
  {mutex = Mutex.create (); changed = Condition.create (); state = Waiting}

let transition gate state =
  Process_child.with_lock gate.mutex (fun () ->
      match gate.state with
      | Waiting ->
        gate.state <- state;
        Condition.broadcast gate.changed
      | Open | Closed -> ())

let open_ gate =
  (match Process_child.with_lock gate.mutex (fun () -> gate.state) with
  | Waiting -> ()
  | Open | Closed -> invalid_arg "speculation gate is not waiting");
  transition gate Open

let abandon gate = transition gate Closed
let cancel = abandon

let is_abandoned gate =
  Process_child.with_lock gate.mutex (fun () -> gate.state = Closed)

let wait gate =
  let state =
    Process_child.with_lock gate.mutex (fun () ->
        while gate.state = Waiting do
          Condition.wait gate.changed gate.mutex
        done;
        gate.state)
  in
  match state with
  | Open -> ()
  | Waiting | Closed -> raise Abandoned

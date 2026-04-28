(** State map for computed OptionalArgs.
    Maps declaration position to final state after all calls/combines. *)

type t = OptionalArgs.t PosHash.t

let create () : t = PosHash.create 256

let find_opt (state : t) pos = PosHash.find_opt state pos

let set (state : t) pos value = PosHash.replace state pos value

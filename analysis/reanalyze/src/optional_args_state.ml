(** State map for computed OptionalArgs.
    Maps declaration position to final state after all calls/combines. *)

type t = Optional_args.t Pos_hash.t

let create () : t = Pos_hash.create 256

let find_opt (state : t) pos = Pos_hash.find_opt state pos

let set (state : t) pos value = Pos_hash.replace state pos value

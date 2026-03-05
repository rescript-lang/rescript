(** Zero-allocation unboxed optional values.

    Internally, [none] is a physically unique sentinel object.
    [some v] is just [Obj.repr v] — no allocation.
    [is_some] checks physical inequality with the sentinel. *)

type 'a t = Obj.t

let sentinel : Obj.t = Obj.repr (ref ())

let none = sentinel
let[@inline] some (x : 'a) : 'a t = Obj.repr x
let[@inline] is_none (x : 'a t) = x == sentinel
let[@inline] is_some (x : 'a t) = x != sentinel
let[@inline] unsafe_get (x : 'a t) : 'a = Obj.obj x
let[@inline] to_option (x : 'a t) : 'a option =
  if x != sentinel then Some (Obj.obj x) else None

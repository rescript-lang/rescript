(** Zero-allocation unboxed optional values.

    Internally, [none] is a physically unique sentinel object.
    [some v] is just [Obj.repr v] — no allocation.
    [is_some] checks physical inequality with the sentinel. *)

type 'a t = Obj.t

let sentinel_words = 257
let sentinel : Obj.t = Obj.repr (Array.make sentinel_words 0)

let none = sentinel
let none_offheap = Offheap.of_value none
let[@inline] some (x : 'a) : 'a t = Obj.repr x
let[@inline] is_none (x : 'a t) = x == sentinel
let[@inline] is_some (x : 'a t) = x != sentinel
let[@inline] unsafe_get (x : 'a t) : 'a = Obj.obj x
let[@inline] maybe_int_to_offheap (x : int t) : int t Offheap.t =
  Offheap.unsafe_of_value x

let[@inline] maybe_unit_to_offheap (x : unit t) : unit t Offheap.t =
  Offheap.unsafe_of_value x

let[@inline] maybe_offheap_list_to_offheap (x : 'a OffheapList.t t) :
    'a list t Offheap.t =
  Offheap.unsafe_of_value x

let[@inline] to_option (x : 'a t) : 'a option =
  if x != sentinel then Some (Obj.obj x) else None

type ('k, 'v) t = (Obj.t, int, int) Allocator.Block2.t

let entry_width = 2

let length (t : ('k, 'v) t) : int = Allocator.Block2.get0 t

let set_length (t : ('k, 'v) t) len = Allocator.Block2.set0 t len

let create ?(max_entries = 16) () : ('k, 'v) t =
  if max_entries < 0 then
    invalid_arg "ReactiveWave.create: max_entries must be >= 0";
  let t =
    Allocator.Block2.create ~capacity:(max_entries * entry_width) ~x0:0 ~y0:0
  in
  set_length t 0;
  t

let clear (t : ('k, 'v) t) = set_length t 0

let destroy (t : ('k, 'v) t) = Allocator.Block2.destroy t

let ensure_capacity (t : ('k, 'v) t) needed =
  let current = Allocator.Block2.capacity t / entry_width in
  if needed > current then (
    let next = ref (max 1 current) in
    while !next < needed do
      next := !next * 2
    done;
    Allocator.Block2.resize t ~capacity:(!next * entry_width))

let push (type k v) (t : (k, v) t) (k : k Stable.t) (v : v Stable.t) =
  let len = length t in
  ensure_capacity t (len + 1);
  let key_slot = len * entry_width in
  Allocator.Block2.set t key_slot (Obj.magic k);
  Allocator.Block2.set t (key_slot + 1) (Obj.magic v);
  set_length t (len + 1)

let iter (type k v) (t : (k, v) t) (f : k Stable.t -> v Stable.t -> unit) =
  let len = length t in
  for i = 0 to len - 1 do
    let key_slot = i * entry_width in
    f
      (Obj.magic (Allocator.Block2.get t key_slot))
      (Obj.magic (Allocator.Block2.get t (key_slot + 1)))
  done

let iter_with (type a k v) (t : (k, v) t)
    (f : a -> k Stable.t -> v Stable.t -> unit) (arg : a) =
  let len = length t in
  for i = 0 to len - 1 do
    let key_slot = i * entry_width in
    f arg
      (Obj.magic (Allocator.Block2.get t key_slot))
      (Obj.magic (Allocator.Block2.get t (key_slot + 1)))
  done

let count (t : ('k, 'v) t) = length t

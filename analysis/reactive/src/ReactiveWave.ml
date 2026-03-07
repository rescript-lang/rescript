type ('k, 'v) t = Allocator.Block.t

let length_slot = 0
let data_offset = 1
let entry_width = 2

let length t : int =
  Allocator.unsafe_from_offheap (Allocator.Block.get t length_slot)

let set_length t len =
  Allocator.Block.set t length_slot (Allocator.int_to_offheap len)

let create ?(max_entries = 16) () =
  if max_entries < 0 then
    invalid_arg "ReactiveWave.create: max_entries must be >= 0";
  let t =
    Allocator.Block.create ~capacity:(data_offset + (max_entries * entry_width))
  in
  set_length t 0;
  t

let clear t = set_length t 0

let destroy t = Allocator.Block.destroy t

let ensure_capacity t needed =
  let current = (Allocator.Block.capacity t - data_offset) / entry_width in
  if needed > current then (
    let next = ref (max 1 current) in
    while !next < needed do
      next := !next * 2
    done;
    Allocator.Block.resize t ~capacity:(data_offset + (!next * entry_width)))

let push (type k v) (t : (k, v) t) (k : k Allocator.offheap)
    (v : v Allocator.offheap) =
  let len = length t in
  ensure_capacity t (len + 1);
  let key_slot = data_offset + (len * entry_width) in
  Allocator.Block.set t key_slot k;
  Allocator.Block.set t (key_slot + 1) v;
  set_length t (len + 1)

let iter (type k v) (t : (k, v) t)
    (f : k Allocator.offheap -> v Allocator.offheap -> unit) =
  let len = length t in
  for i = 0 to len - 1 do
    let key_slot = data_offset + (i * entry_width) in
    f (Allocator.Block.get t key_slot) (Allocator.Block.get t (key_slot + 1))
  done

let iter_with (type a k v) (t : (k, v) t)
    (f : a -> k Allocator.offheap -> v Allocator.offheap -> unit) (arg : a) =
  let len = length t in
  for i = 0 to len - 1 do
    let key_slot = data_offset + (i * entry_width) in
    f arg
      (Allocator.Block.get t key_slot)
      (Allocator.Block.get t (key_slot + 1))
  done

let count t = length t

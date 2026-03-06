type ('k, 'v) t = ReactiveAllocator.Block.t

let length_slot = 0
let data_offset = 1
let entry_width = 2

let length t : int =
  ReactiveAllocator.unsafe_from_offheap
    (ReactiveAllocator.Block.get t length_slot)

let set_length t len =
  ReactiveAllocator.Block.set t length_slot
    (ReactiveAllocator.int_to_offheap len)

let create ?(max_entries = 16) () =
  if max_entries < 0 then
    invalid_arg "ReactiveWave.create: max_entries must be >= 0";
  let t =
    ReactiveAllocator.Block.create
      ~capacity:(data_offset + (max_entries * entry_width))
  in
  set_length t 0;
  t

let clear t = set_length t 0

let destroy t = ReactiveAllocator.Block.destroy t

let ensure_capacity t needed =
  let current =
    (ReactiveAllocator.Block.capacity t - data_offset) / entry_width
  in
  if needed > current then (
    let next = ref (max 1 current) in
    while !next < needed do
      next := !next * 2
    done;
    ReactiveAllocator.Block.resize t
      ~capacity:(data_offset + (!next * entry_width)))

let push (type k v) (t : (k, v) t) (k : k ReactiveAllocator.offheap)
    (v : v ReactiveAllocator.offheap) =
  let len = length t in
  ensure_capacity t (len + 1);
  let key_slot = data_offset + (len * entry_width) in
  ReactiveAllocator.Block.set t key_slot k;
  ReactiveAllocator.Block.set t (key_slot + 1) v;
  set_length t (len + 1)

let iter (type k v) (t : (k, v) t)
    (f : k ReactiveAllocator.offheap -> v ReactiveAllocator.offheap -> unit) =
  let len = length t in
  for i = 0 to len - 1 do
    let key_slot = data_offset + (i * entry_width) in
    f
      (ReactiveAllocator.Block.get t key_slot)
      (ReactiveAllocator.Block.get t (key_slot + 1))
  done

let iter_with (type a k v) (t : (k, v) t)
    (f :
      a -> k ReactiveAllocator.offheap -> v ReactiveAllocator.offheap -> unit)
    (arg : a) =
  let len = length t in
  for i = 0 to len - 1 do
    let key_slot = data_offset + (i * entry_width) in
    f arg
      (ReactiveAllocator.Block.get t key_slot)
      (ReactiveAllocator.Block.get t (key_slot + 1))
  done

let count t = length t

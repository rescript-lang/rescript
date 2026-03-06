type 'a t = ReactiveAllocator.Block.t

let length_slot = 0
let data_offset = 1

let length t : int =
  ReactiveAllocator.unsafe_from_offheap
    (ReactiveAllocator.Block.get t length_slot)

let capacity t = ReactiveAllocator.Block.capacity t - data_offset

let create ~initial_capacity =
  if initial_capacity < 0 then invalid_arg "ReactiveTable.create";
  let t =
    ReactiveAllocator.Block.create ~capacity:(initial_capacity + data_offset)
  in
  ReactiveAllocator.Block.set t length_slot (ReactiveAllocator.int_to_offheap 0);
  t

let destroy = ReactiveAllocator.Block.destroy

let clear t =
  ReactiveAllocator.Block.set t length_slot (ReactiveAllocator.int_to_offheap 0)

let ensure_capacity t needed =
  let old_capacity = capacity t in
  if needed > old_capacity then (
    let new_capacity = ref (max 1 old_capacity) in
    while !new_capacity < needed do
      new_capacity := !new_capacity * 2
    done;
    ReactiveAllocator.Block.resize t ~capacity:(!new_capacity + data_offset))

let get t index =
  let len = length t in
  if index < 0 || index >= len then invalid_arg "ReactiveTable.get";
  ReactiveAllocator.Block.get t (index + data_offset)

let set t index value =
  let len = length t in
  if index < 0 || index >= len then invalid_arg "ReactiveTable.set";
  ReactiveAllocator.Block.set t (index + data_offset) value

let push t value =
  let len = length t in
  let next_len = len + 1 in
  ensure_capacity t next_len;
  ReactiveAllocator.Block.set t (len + data_offset) value;
  ReactiveAllocator.Block.set t length_slot
    (ReactiveAllocator.int_to_offheap next_len)

let pop t =
  let len = length t in
  if len = 0 then invalid_arg "ReactiveTable.pop";
  let last = ReactiveAllocator.Block.get t (len - 1 + data_offset) in
  ReactiveAllocator.Block.set t length_slot
    (ReactiveAllocator.int_to_offheap (len - 1));
  last

let shrink_to_fit t =
  ReactiveAllocator.Block.resize t ~capacity:(length t + data_offset)

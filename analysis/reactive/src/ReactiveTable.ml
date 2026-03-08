type 'a t = Obj.t Allocator.Block.t

let length_slot = 0
let data_offset = 1

let length (t : 'a t) : int = Obj.magic (Allocator.Block.get t length_slot)

let capacity (t : 'a t) = Allocator.Block.capacity t - data_offset

let create ~initial_capacity : 'a t =
  if initial_capacity < 0 then invalid_arg "ReactiveTable.create";
  let t = Allocator.Block.create ~capacity:(initial_capacity + data_offset) in
  Allocator.Block.set t length_slot (Obj.magic (Offheap.int 0));
  t

let destroy = Allocator.Block.destroy

let clear (t : 'a t) =
  Allocator.Block.set t length_slot (Obj.magic (Offheap.int 0))

let ensure_capacity (t : 'a t) needed =
  let old_capacity = capacity t in
  if needed > old_capacity then (
    let new_capacity = ref (max 1 old_capacity) in
    while !new_capacity < needed do
      new_capacity := !new_capacity * 2
    done;
    Allocator.Block.resize t ~capacity:(!new_capacity + data_offset))

let get (t : 'a t) index =
  let len = length t in
  if index < 0 || index >= len then invalid_arg "ReactiveTable.get";
  Obj.magic (Allocator.Block.get t (index + data_offset))

let set (t : 'a t) index value =
  let len = length t in
  if index < 0 || index >= len then invalid_arg "ReactiveTable.set";
  Allocator.Block.set t (index + data_offset) (Obj.magic value)

let push (t : 'a t) value =
  let len = length t in
  let next_len = len + 1 in
  ensure_capacity t next_len;
  Allocator.Block.set t (len + data_offset) (Obj.magic value);
  Allocator.Block.set t length_slot (Obj.magic (Offheap.int next_len))

let pop (t : 'a t) =
  let len = length t in
  if len = 0 then invalid_arg "ReactiveTable.pop";
  let last = Obj.magic (Allocator.Block.get t (len - 1 + data_offset)) in
  Allocator.Block.set t length_slot (Obj.magic (Offheap.int (len - 1)));
  last

let shrink_to_fit (t : 'a t) =
  Allocator.Block.resize t ~capacity:(length t + data_offset)

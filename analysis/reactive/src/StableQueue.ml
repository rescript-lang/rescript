(* Representation of ['a t]:

   - ['a t] is [('a, int, int) Allocator.Block2.t].
   - Header slot [0]: head index.
   - Header slot [1]: tail index.
   - Data slots: queue elements, stored as ['a Stable.t].

   Head and tail are monotone counters. Physical slot positions are computed
   from the current capacity via bit masking, so the backing capacity always
   stays a power of two. *)

type 'a t = ('a, int, int) Allocator.Block2.t

let initial_capacity = 16

let head = Allocator.Block2.get0
let set_head = Allocator.Block2.set0
let tail = Allocator.Block2.get1
let set_tail = Allocator.Block2.set1
let slot_capacity = Allocator.Block2.capacity

let create () = Allocator.Block2.create ~capacity:initial_capacity ~x0:0 ~y0:0

let destroy = Allocator.Block2.destroy

let clear t =
  set_head t 0;
  set_tail t 0

let[@inline] is_empty t = head t = tail t
let[@inline] length t = tail t - head t
let[@inline] mask t = slot_capacity t - 1
let[@inline] slot_index t i = i land mask t

let resize (type a) (t : a t) new_cap =
  let old_len = length t in
  let fresh = Allocator.Block2.create ~capacity:new_cap ~x0:0 ~y0:old_len in
  for i = 0 to old_len - 1 do
    let src = slot_index t (head t + i) in
    Allocator.Block2.set fresh i (Allocator.Block2.get t src)
  done;
  Allocator.Block2.blit ~src:fresh ~src_pos:0 ~dst:t ~dst_pos:0 ~len:new_cap;
  Allocator.Block2.destroy fresh

let maybe_grow_before_push t =
  if length t = slot_capacity t then resize t (2 * slot_capacity t)

let push t x =
  maybe_grow_before_push t;
  let tail_i = tail t in
  Allocator.Block2.set t (slot_index t tail_i) x;
  set_tail t (tail_i + 1)

let pop t =
  if is_empty t then invalid_arg "StableQueue.pop: empty";
  let head_i = head t in
  let x = Allocator.Block2.get t (slot_index t head_i) in
  set_head t (head_i + 1);
  x

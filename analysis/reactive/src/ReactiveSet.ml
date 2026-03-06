(* Representation of ['a t]:

   - ['a t] is [('a, int, int) ReactiveAllocator.Block2.t].
   - Header slot [0]: population, exposed as [int].
   - Header slot [1]: index mask, exposed as [int].
   - Data slots: keys, stored as ['a ReactiveAllocator.offheap].

   The backing block lives off-heap. Elements are ordinary OCaml values whose
   storage invariant has already been established before insertion.

   Empty data slots contain a distinguished sentinel value. All other data
   slots contain real set elements. *)

type 'a t = ('a, int, int) ReactiveAllocator.Block2.t

let initial_capacity = 8
let max_load_percent = 82

let sentinel : Obj.t = Obj.repr (Array.make 257 0)
let[@inline] empty_sentinel =
 fun () -> (Obj.magic sentinel : 'a ReactiveAllocator.offheap)

let slot_capacity = ReactiveAllocator.Block2.capacity
let population = ReactiveAllocator.Block2.get0
let set_population = ReactiveAllocator.Block2.set0
let mask = ReactiveAllocator.Block2.get1
let set_mask = ReactiveAllocator.Block2.set1

let[@inline] start t x =
  Hashtbl.hash (ReactiveAllocator.unsafe_from_offheap x) land mask t
let[@inline] next t j = (j + 1) land mask t

let[@inline] crowded_or_full pop cap = 100 * pop > max_load_percent * cap

let clear_slots t =
  for i = 0 to slot_capacity t - 1 do
    ReactiveAllocator.Block2.set t i (empty_sentinel ())
  done

let create () =
  let t =
    ReactiveAllocator.Block2.create ~capacity:initial_capacity ~x0:0
      ~y0:(initial_capacity - 1)
  in
  clear_slots t;
  t

let destroy = ReactiveAllocator.Block2.destroy

let clear t =
  set_population t 0;
  clear_slots t

let add_absent_key (type a) (t : a t) (x : a ReactiveAllocator.offheap) =
  let j = ref (start t x) in
  while ReactiveAllocator.Block2.get t !j != empty_sentinel () do
    j := next t !j
  done;
  ReactiveAllocator.Block2.set t !j x

let resize (type a) (t : a t) new_cap =
  let old_cap = slot_capacity t in
  let old_keys : a t =
    ReactiveAllocator.Block2.create ~capacity:old_cap ~x0:0 ~y0:(old_cap - 1)
  in
  ReactiveAllocator.Block2.blit ~src:t ~src_pos:0 ~dst:old_keys ~dst_pos:0
    ~len:old_cap;
  ReactiveAllocator.Block2.resize t ~capacity:new_cap;
  clear_slots t;
  set_mask t (new_cap - 1);
  for i = 0 to old_cap - 1 do
    let x = ReactiveAllocator.Block2.get old_keys i in
    if x != empty_sentinel () then add_absent_key t x
  done;
  ReactiveAllocator.Block2.destroy old_keys

let maybe_grow_before_add (type a) (t : a t) =
  let cap = slot_capacity t in
  let next_population = population t + 1 in
  if crowded_or_full next_population cap then resize t (2 * cap)

let add (type a) (t : a t) (x : a ReactiveAllocator.offheap) =
  maybe_grow_before_add t;
  let j = ref (start t x) in
  let found = ref false in
  while not !found do
    let current = ReactiveAllocator.Block2.get t !j in
    if current == empty_sentinel () then (
      ReactiveAllocator.Block2.set t !j x;
      set_population t (population t + 1);
      found := true)
    else if current = x then found := true
    else j := next t !j
  done

let iter_with (type a k) (f : a -> k ReactiveAllocator.offheap -> unit)
    (arg : a) (t : k t) =
  if population t > 0 then
    for i = 0 to slot_capacity t - 1 do
      let x = ReactiveAllocator.Block2.get t i in
      if x != empty_sentinel () then f arg x
    done

let cardinal = population

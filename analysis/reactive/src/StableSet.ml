(* Representation of ['a t]:

   - ['a t] is [('a, int, int) Allocator.Block2.t].
   - Header slot [0]: population, exposed as [int].
   - Header slot [1]: index mask, exposed as [int].
   - Data slots: keys, stored as ['a Stable.t].

   The backing block lives stable. Elements are ordinary OCaml values whose
   storage invariant has already been established before insertion.

   Data slots contain either:
   - the distinguished empty sentinel, meaning the slot has never been used
   - the distinguished tomb sentinel, meaning the slot was removed
   - a real set element. *)

type 'a t = ('a, int, int) Allocator.Block2.t

let initial_capacity = 8
let max_load_percent = 82

let sentinel : Obj.t = Obj.repr (Array.make 257 0)
let tomb : Obj.t = Obj.repr (Array.make 257 0)
let[@inline] empty_sentinel = fun () -> (Obj.magic sentinel : 'a Stable.t)
let[@inline] tomb_sentinel = fun () -> (Obj.magic tomb : 'a Stable.t)

let slot_capacity = Allocator.Block2.capacity
let population = Allocator.Block2.get0
let set_population = Allocator.Block2.set0
let mask = Allocator.Block2.get1
let set_mask = Allocator.Block2.set1

let[@inline] start t x = Hashtbl.hash (Stable.unsafe_to_value x) land mask t
let[@inline] next t j = (j + 1) land mask t

let[@inline] crowded_or_full pop cap = 100 * pop > max_load_percent * cap

let clear_slots t =
  for i = 0 to slot_capacity t - 1 do
    Allocator.Block2.set t i (empty_sentinel ())
  done

let create () =
  let t =
    Allocator.Block2.create ~capacity:initial_capacity ~x0:0
      ~y0:(initial_capacity - 1)
  in
  clear_slots t;
  t

let destroy = Allocator.Block2.destroy

let clear t =
  set_population t 0;
  clear_slots t

let add_absent_key (type a) (t : a t) (x : a Stable.t) =
  let j = ref (start t x) in
  while
    let current = Allocator.Block2.get t !j in
    current != empty_sentinel () && current != tomb_sentinel ()
  do
    j := next t !j
  done;
  Allocator.Block2.set t !j x

let resize (type a) (t : a t) new_cap =
  let old_cap = slot_capacity t in
  let old_keys : a t =
    Allocator.Block2.create ~capacity:old_cap ~x0:0 ~y0:(old_cap - 1)
  in
  Allocator.Block2.blit ~src:t ~src_pos:0 ~dst:old_keys ~dst_pos:0 ~len:old_cap;
  Allocator.Block2.resize t ~capacity:new_cap;
  clear_slots t;
  set_mask t (new_cap - 1);
  for i = 0 to old_cap - 1 do
    let x = Allocator.Block2.get old_keys i in
    if x != empty_sentinel () && x != tomb_sentinel () then add_absent_key t x
  done;
  Allocator.Block2.destroy old_keys

let maybe_grow_before_add (type a) (t : a t) =
  let cap = slot_capacity t in
  let next_population = population t + 1 in
  if crowded_or_full next_population cap then resize t (2 * cap)

let add (type a) (t : a t) (x : a Stable.t) =
  maybe_grow_before_add t;
  let j = ref (start t x) in
  let first_tomb = ref (-1) in
  let found = ref false in
  while not !found do
    let current = Allocator.Block2.get t !j in
    if current == empty_sentinel () then (
      let dst = if !first_tomb >= 0 then !first_tomb else !j in
      Allocator.Block2.set t dst x;
      set_population t (population t + 1);
      found := true)
    else if current == tomb_sentinel () then (
      if !first_tomb < 0 then first_tomb := !j;
      j := next t !j)
    else if current = x then found := true
    else j := next t !j
  done

let remove (type a) (t : a t) (x : a Stable.t) =
  let j = ref (start t x) in
  let done_ = ref false in
  while not !done_ do
    let current = Allocator.Block2.get t !j in
    if current == empty_sentinel () then done_ := true
    else if current == tomb_sentinel () then j := next t !j
    else if current = x then (
      Allocator.Block2.set t !j (tomb_sentinel ());
      set_population t (population t - 1);
      done_ := true)
    else j := next t !j
  done

let mem (type a) (t : a t) (x : a Stable.t) =
  let j = ref (start t x) in
  let found = ref false in
  let done_ = ref false in
  while not !done_ do
    let current = Allocator.Block2.get t !j in
    if current == empty_sentinel () then done_ := true
    else if current == tomb_sentinel () then j := next t !j
    else if current = x then (
      found := true;
      done_ := true)
    else j := next t !j
  done;
  !found

let iter_with (type a k) (f : a -> k Stable.t -> unit) (arg : a) (t : k t) =
  if population t > 0 then
    for i = 0 to slot_capacity t - 1 do
      let x = Allocator.Block2.get t i in
      if x != empty_sentinel () && x != tomb_sentinel () then f arg x
    done

let exists_with (type a k) (f : a -> k Stable.t -> bool) (arg : a) (t : k t) =
  let found = ref false in
  let done_ = ref false in
  let i = ref 0 in
  while not !done_ && !i < slot_capacity t do
    let x = Allocator.Block2.get t !i in
    if x != empty_sentinel () && x != tomb_sentinel () && f arg x then (
      found := true;
      done_ := true);
    incr i
  done;
  !found

let cardinal = population

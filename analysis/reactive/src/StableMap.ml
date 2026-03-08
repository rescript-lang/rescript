type ('k, 'v) t = (Obj.t, int, int) Allocator.Block2.t

let initial_capacity = 8
let max_load_percent = 82

let empty_sentinel : Obj.t = Obj.repr (Array.make 257 0)
let tomb_sentinel : Obj.t = Obj.repr (Array.make 257 0)

let[@inline] empty_slot () : 'a Stable.t = Obj.magic empty_sentinel
let[@inline] tomb_slot () : 'a Stable.t = Obj.magic tomb_sentinel

let[@inline] pair_capacity t = Allocator.Block2.capacity t / 2
let population = Allocator.Block2.get0
let set_population = Allocator.Block2.set0
let occupation = Allocator.Block2.get1
let set_occupation = Allocator.Block2.set1
let[@inline] mask t = pair_capacity t - 1

let[@inline] key_slot j = 2 * j
let[@inline] val_slot j = (2 * j) + 1

let[@inline] get_key t j : 'k Stable.t =
  Obj.magic (Allocator.Block2.get t (key_slot j))

let[@inline] set_key t j (k : 'k Stable.t) =
  Allocator.Block2.set t (key_slot j) (Obj.magic k)

let[@inline] get_val t j : 'v Stable.t =
  Obj.magic (Allocator.Block2.get t (val_slot j))

let[@inline] set_val t j (v : 'v Stable.t) =
  Allocator.Block2.set t (val_slot j) (Obj.magic v)

let[@inline] start t x = Hashtbl.hash (Stable.unsafe_to_value x) land mask t
let[@inline] next t j = (j + 1) land mask t
let[@inline] crowded_or_full occ cap = 100 * occ > max_load_percent * cap

let clear_keys t =
  for i = 0 to pair_capacity t - 1 do
    set_key t i (empty_slot ())
  done

let create () =
  let t =
    Allocator.Block2.create ~capacity:(2 * initial_capacity) ~x0:0 ~y0:0
  in
  clear_keys t;
  t

let destroy = Allocator.Block2.destroy

let clear t =
  set_population t 0;
  set_occupation t 0;
  clear_keys t

let insert_absent t k v =
  let empty : 'k Stable.t = empty_slot () in
  let j = ref (start t k) in
  while get_key t !j != empty do
    j := next t !j
  done;
  set_key t !j k;
  set_val t !j v

let resize t new_cap =
  let old_cap = pair_capacity t in
  let old =
    Allocator.Block2.create ~capacity:(2 * old_cap) ~x0:0 ~y0:0
  in
  Allocator.Block2.blit ~src:t ~src_pos:0 ~dst:old ~dst_pos:0 ~len:(2 * old_cap);
  Allocator.Block2.resize t ~capacity:(2 * new_cap);
  set_population t 0;
  set_occupation t 0;
  clear_keys t;
  for i = 0 to old_cap - 1 do
    let k = get_key old i in
    if k != empty_slot () && k != tomb_slot () then (
      insert_absent t k (get_val old i);
      set_population t (population t + 1);
      set_occupation t (occupation t + 1))
  done;
  Allocator.Block2.destroy old

let maybe_grow_before_insert t =
  let cap = pair_capacity t in
  let next_occupation = occupation t + 1 in
  if crowded_or_full next_occupation cap then resize t (2 * cap)

let replace t k v =
  maybe_grow_before_insert t;
  let empty : 'k Stable.t = empty_slot () in
  let tomb : 'k Stable.t = tomb_slot () in
  let j = ref (start t k) in
  let first_tomb = ref (-1) in
  let done_ = ref false in
  while not !done_ do
    let current = get_key t !j in
    if current == empty then (
      let dst = if !first_tomb >= 0 then !first_tomb else !j in
      if !first_tomb < 0 then set_occupation t (occupation t + 1);
      set_population t (population t + 1);
      set_key t dst k;
      set_val t dst v;
      done_ := true)
    else if current == tomb then (
      if !first_tomb < 0 then first_tomb := !j;
      j := next t !j)
    else if current = k then (
      set_val t !j v;
      done_ := true)
    else j := next t !j
  done

let remove t k =
  let empty : 'k Stable.t = empty_slot () in
  let tomb : 'k Stable.t = tomb_slot () in
  let j = ref (start t k) in
  let done_ = ref false in
  while not !done_ do
    let current = get_key t !j in
    if current == empty then done_ := true
    else if current == tomb then j := next t !j
    else if current = k then (
      set_key t !j tomb;
      set_population t (population t - 1);
      done_ := true)
    else j := next t !j
  done

let mem t k =
  let empty : 'k Stable.t = empty_slot () in
  let tomb : 'k Stable.t = tomb_slot () in
  let j = ref (start t k) in
  let found = ref false in
  let done_ = ref false in
  while not !done_ do
    let current = get_key t !j in
    if current == empty then done_ := true
    else if current == tomb then j := next t !j
    else if current = k then (
      found := true;
      done_ := true)
    else j := next t !j
  done;
  !found

let find_maybe t k =
  let empty : 'k Stable.t = empty_slot () in
  let tomb : 'k Stable.t = tomb_slot () in
  let j = ref (start t k) in
  let found = ref Maybe.none in
  let done_ = ref false in
  while not !done_ do
    let current = get_key t !j in
    if current == empty then done_ := true
    else if current == tomb then j := next t !j
    else if current = k then (
      found := Maybe.some (get_val t !j);
      done_ := true)
    else j := next t !j
  done;
  !found

let iter_with f arg t =
  let empty : 'k Stable.t = empty_slot () in
  let tomb : 'k Stable.t = tomb_slot () in
  if population t > 0 then
    for i = 0 to pair_capacity t - 1 do
      let k = get_key t i in
      if k != empty && k != tomb then f arg k (get_val t i)
    done

let iter f t = iter_with (fun f k v -> f k v) f t
let cardinal = population

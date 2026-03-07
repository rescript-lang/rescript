(** Array-based FIFO queue. After [clear], subsequent [push] calls
    reuse existing array slots — zero allocation until the array
    needs to grow beyond its high-water mark. *)

type 'a t = {
  mutable data: Obj.t array;
  mutable head: int;
  mutable tail: int; (* next write position *)
}

let create () = {data = [||]; head = 0; tail = 0}
let clear t =
  t.head <- 0;
  t.tail <- 0
let is_empty t = t.head = t.tail

let grow t =
  let old_len = Array.length t.data in
  let used = t.tail - t.head in
  let new_len = max 16 (old_len * 2) in
  let new_data = Array.make new_len (Obj.repr ()) in
  Array.blit t.data t.head new_data 0 used;
  t.data <- new_data;
  t.head <- 0;
  t.tail <- used

let push t (x : 'a) =
  if t.tail >= Array.length t.data then grow t;
  Array.unsafe_set t.data t.tail (Obj.repr x);
  t.tail <- t.tail + 1

let pop t =
  if t.head = t.tail then invalid_arg "ReactiveQueue.pop: empty";
  let v = Array.unsafe_get t.data t.head in
  t.head <- t.head + 1;
  (Obj.obj v : 'a)

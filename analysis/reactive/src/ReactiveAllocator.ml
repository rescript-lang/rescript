type 'a offheap = 'a

external slot_size_bytes_unsafe : unit -> int =
  "caml_reactive_allocator_slot_size_bytes"
[@@noalloc]
external live_block_count : unit -> int = "caml_reactive_allocator_live_block_count"
[@@noalloc]

external live_block_capacity_slots : unit -> int =
  "caml_reactive_allocator_live_block_capacity_slots"
[@@noalloc]
external is_in_minor_heap : 'a -> bool = "caml_reactive_value_is_young"
[@@noalloc]

let check_non_negative name n =
  if n < 0 then invalid_arg name

let slot_size_bytes = slot_size_bytes_unsafe ()
let unsafe_to_offheap x = x
let unsafe_from_offheap x = x
let int_to_offheap x = unsafe_to_offheap x
let unit_to_offheap x = unsafe_to_offheap x

let to_offheap x =
  if is_in_minor_heap x then invalid_arg "ReactiveAllocator.to_offheap";
  unsafe_to_offheap x

module Block = struct
  type t = int

  external create_unsafe : int -> t = "caml_reactive_allocator_create"
  [@@noalloc]

  external destroy : t -> unit = "caml_reactive_allocator_destroy" [@@noalloc]
  external capacity : t -> int = "caml_reactive_allocator_capacity" [@@noalloc]
  external resize_unsafe : t -> int -> unit = "caml_reactive_allocator_resize"
  [@@noalloc]

  external unsafe_get : t -> int -> 'a offheap = "caml_reactive_allocator_get"
  [@@noalloc]

  external unsafe_set : t -> int -> 'a offheap -> unit =
    "caml_reactive_allocator_set"
  [@@noalloc]

  external blit_unsafe :
    t -> int -> t -> int -> int -> unit = "caml_reactive_allocator_blit"
  [@@noalloc]

  let create ~capacity =
    check_non_negative "ReactiveAllocator.Block.create" capacity;
    create_unsafe capacity

  let resize block ~capacity =
    check_non_negative "ReactiveAllocator.Block.resize" capacity;
    resize_unsafe block capacity

  let get block index =
    let cap = capacity block in
    if index < 0 || index >= cap then invalid_arg "ReactiveAllocator.Block.get";
    unsafe_get block index

  let set block index value =
    let cap = capacity block in
    if index < 0 || index >= cap then invalid_arg "ReactiveAllocator.Block.set";
    unsafe_set block index value

  let blit ~src ~src_pos ~dst ~dst_pos ~len =
    check_non_negative "ReactiveAllocator.Block.blit" src_pos;
    check_non_negative "ReactiveAllocator.Block.blit" dst_pos;
    check_non_negative "ReactiveAllocator.Block.blit" len;
    let src_cap = capacity src in
    let dst_cap = capacity dst in
    if src_pos + len > src_cap || dst_pos + len > dst_cap then
      invalid_arg "ReactiveAllocator.Block.blit";
    blit_unsafe src src_pos dst dst_pos len
end

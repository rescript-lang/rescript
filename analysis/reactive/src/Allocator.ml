external slot_size_bytes_unsafe : unit -> int
  = "caml_reactive_allocator_slot_size_bytes"
[@@noalloc]
external live_block_count : unit -> int
  = "caml_reactive_allocator_live_block_count"
[@@noalloc]

external live_block_capacity_slots : unit -> int
  = "caml_reactive_allocator_live_block_capacity_slots"
[@@noalloc]
external reset : unit -> unit = "caml_reactive_allocator_reset" [@@noalloc]
external is_in_minor_heap : 'a -> bool = "caml_reactive_value_is_young"
[@@noalloc]

let check_non_negative name n = if n < 0 then invalid_arg name

let slot_size_bytes = slot_size_bytes_unsafe ()

module Block = struct
  type 'a t = int

  external create_unsafe : int -> 'a t = "caml_reactive_allocator_create"
  [@@noalloc]

  external destroy : 'a t -> unit = "caml_reactive_allocator_destroy"
  [@@noalloc]

  external capacity : 'a t -> int = "caml_reactive_allocator_capacity"
  [@@noalloc]

  external resize_unsafe : 'a t -> int -> unit
    = "caml_reactive_allocator_resize"
  [@@noalloc]

  external unsafe_get : 'a t -> int -> 'a Stable.t
    = "caml_reactive_allocator_get"
  [@@noalloc]

  external unsafe_set : 'a t -> int -> 'a Stable.t -> unit
    = "caml_reactive_allocator_set"
  [@@noalloc]

  external blit_unsafe : 'a t -> int -> 'a t -> int -> int -> unit
    = "caml_reactive_allocator_blit"
  [@@noalloc]

  let create ~capacity =
    check_non_negative "Allocator.Block.create" capacity;
    create_unsafe capacity

  let resize block ~capacity =
    check_non_negative "Allocator.Block.resize" capacity;
    resize_unsafe block capacity

  let get block index =
    let cap = capacity block in
    if index < 0 || index >= cap then invalid_arg "Allocator.Block.get";
    unsafe_get block index

  let set block index value =
    let cap = capacity block in
    if index < 0 || index >= cap then invalid_arg "Allocator.Block.set";
    unsafe_set block index value

  let blit ~src ~src_pos ~dst ~dst_pos ~len =
    check_non_negative "Allocator.Block.blit" src_pos;
    check_non_negative "Allocator.Block.blit" dst_pos;
    check_non_negative "Allocator.Block.blit" len;
    let src_cap = capacity src in
    let dst_cap = capacity dst in
    if src_pos + len > src_cap || dst_pos + len > dst_cap then
      invalid_arg "Allocator.Block.blit";
    blit_unsafe src src_pos dst dst_pos len
end

module Block2 = struct
  type ('a, 'x, 'y) t = 'a Block.t

  let header_slots = 2

  let create ~capacity ~x0 ~y0 =
    let t = Block.create ~capacity:(capacity + header_slots) in
    Block.set t 0 (Stable.unsafe_of_value x0);
    Block.set t 1 (Stable.unsafe_of_value y0);
    t

  let destroy = Block.destroy
  let capacity t = Block.capacity t - header_slots
  let resize t ~capacity = Block.resize t ~capacity:(capacity + header_slots)
  let get0 t = Stable.to_linear_value (Block.get t 0)
  let set0 t x = Block.set t 0 (Stable.unsafe_of_value x)
  let get1 t = Stable.to_linear_value (Block.get t 1)
  let set1 t y = Block.set t 1 (Stable.unsafe_of_value y)
  let get t index = Block.get t (index + header_slots)
  let set t index value = Block.set t (index + header_slots) value

  let blit ~src ~src_pos ~dst ~dst_pos ~len =
    Block.blit ~src ~src_pos:(src_pos + header_slots) ~dst
      ~dst_pos:(dst_pos + header_slots) ~len
end

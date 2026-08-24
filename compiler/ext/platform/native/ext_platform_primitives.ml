(* Native optimized compiler primitives. *)
external hash_string : string -> int = "caml_bs_hash_string" [@@noalloc]

external hash_string_int : string -> int -> int = "caml_bs_hash_string_and_int"
[@@noalloc]

external hash_stamp_and_name : int -> string -> int
  = "caml_bs_hash_stamp_and_name"
[@@noalloc]

external hash_int : int -> int = "caml_bs_hash_int" [@@noalloc]

external string_length_based_compare : string -> string -> int
  = "caml_string_length_based_compare"
[@@noalloc]

external int_unsafe_blit : int array -> int -> int array -> int -> int -> unit
  = "caml_int_array_blit"
[@@noalloc]

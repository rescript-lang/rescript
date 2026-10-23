(* Portable compiler primitives used by the playground. *)
let hash_string : string -> int = Hashtbl.hash
let hash_string_int string int = Hashtbl.hash (string, int)
let hash_stamp_and_name int string = Hashtbl.hash (int, string)
let hash_int : int -> int = Hashtbl.hash

let string_length_based_compare x y =
  let x_length = String.length x in
  let y_length = String.length y in
  if x_length = y_length then String.compare x y
  else Int.compare x_length y_length

let int_unsafe_blit = Array.blit

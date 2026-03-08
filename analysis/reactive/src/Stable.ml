type 'a t = 'a

external is_in_minor_heap : 'a -> bool = "caml_reactive_value_is_young"
[@@noalloc]

let unsafe_of_value x = x
let unsafe_to_value x = x
let int x = unsafe_of_value x
let unit = unsafe_of_value ()

let of_value x =
  if is_in_minor_heap x then invalid_arg "Stable.of_value";
  unsafe_of_value x

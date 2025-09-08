let min_int x y = if x < y then x else y

let random_string chars upper =
  let len = Array.length chars in
  let string_len = Random.int (min_int upper len) in
  String.init string_len (fun _i -> chars.(Random.int len))

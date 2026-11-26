exception Stack_overflow
exception Sys_error(string)
let _ = (Int.toString, Float.isNaN)

/* make sure exception runtime is there */
let f = x =>
  switch x {
  | Not_found => 0
  | Invalid_argument(_)
  | Stack_overflow => 1
  | Sys_error(_) => 2
  }

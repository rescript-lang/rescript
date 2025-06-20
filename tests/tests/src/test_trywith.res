exception Out_of_memory
exception Sys_error(string)
exception Stack_overflow
exception Sys_blocked_io
let ff = (g, x) => {
  try g(x) catch {
  | Not_found => ()
  }
  try g(x) catch {
  | Out_of_memory => ()
  }
  try g(x) catch {
  | Sys_error(_) => ()
  }
  try g(x) catch {
  | Invalid_argument(_) => ()
  }
  try g(x) catch {
  | End_of_file => ()
  }
  try g(x) catch {
  | Match_failure(_) => ()
  }
  try g(x) catch {
  | Stack_overflow => ()
  }
  try g(x) catch {
  | Sys_blocked_io => ()
  }

  try g(x) catch {
  | Assert_failure(_) => ()
  }
  try g(x) catch {
  | Undefined_recursive_module(_) => ()
  }
}

@@warning("-21")

let u = () => {
  throw(Not_found)
  let f = 3 + 3
  f + f
}
let u1 = "bad character decimal encoding \\"
let v = "bad character decimal encoding \\%c%c%c"

/** test default branch */
type u = A | B | C | D(int) | E(char)

let f = (x: u) =>
  switch x {
  | D(_) => 1
  | A
  | B
  | C => 2

  | _ => assert(false)
  }

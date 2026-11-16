// `open`, `exception` and `external` followed on the same line by an
// identifier, a string or a number are misspelled declarations: a variable is
// never followed by one of these.
open belt
exception foo
external "f": int = "f"
let f = () => {
  open belt
  1
}
let x = 1

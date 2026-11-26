// Payloads are not recognized by their type's name: this unannotated type
// only shares its path with a stdlib class
module Stdlib_Date = {
  type t
}

@unboxed
type t = Date(Stdlib_Date.t) | Text(string)

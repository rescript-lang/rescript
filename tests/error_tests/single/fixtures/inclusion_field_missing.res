// A field missing in the middle is reported as missing, not as a renaming of
// the fields after it
module M: {
  type t = {name: string, age: int, email: string}
} = {
  type t = {name: string, email: string}
}

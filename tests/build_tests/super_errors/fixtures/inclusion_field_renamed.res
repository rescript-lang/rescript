// A field that each declaration lacks after the same field is a renaming
module M: {
  type t = {name: string, email: string, age: int}
} = {
  type t = {name: string, mail: string, age: int}
}

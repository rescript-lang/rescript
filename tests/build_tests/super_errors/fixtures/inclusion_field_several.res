// Every field that differs is reported, not only the first one
module M: {
  type t = {id: int, mutable name: string, age: int, @as("e-mail") email?: string}
} = {
  type t = {id: string, name: string, age: float, email: string}
}

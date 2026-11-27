// The differences of an inline record are reported under its constructor
module M: {
  type t = Person({name: string, age: int}) | Anonymous
} = {
  type t = Person({age: int, name: string, nick: string}) | Anonymous
}

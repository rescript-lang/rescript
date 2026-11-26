module M: {
  type t<'a, 'b, 'c> = {x: int, y: list<('a, 'b)>, z: int}
} = {
  type t<'a, 'b, 'c> = {x: int, y: list<('a, 'c)>, z: int}
}

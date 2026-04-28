open Mocha
open Test_utils

let f = x =>
  switch x() {
  | 1 => 'a'
  | 2 => 'b'
  | 3 => 'c'
  | _ => 'x'
  }

type t = A | B | C | D

let f22 = x =>
  switch x() {
  | 3 => 'c'
  | 2 => 'b'
  | 1 => 'a'
  | _ => 'x'
  }

let f33 = x =>
  switch x() {
  | C => 'c'
  | B => 'b'
  | A => 'a'
  | _ => 'x'
  }

describe(__MODULE__, () => {
  test("integer switch function", () => {
    eq(__LOC__, f(_ => 1), 'a')
    eq(__LOC__, f(_ => 2), 'b')
    eq(__LOC__, f(_ => 3), 'c')
    eq(__LOC__, f(_ => 0), 'x')
    eq(__LOC__, f(_ => -1), 'x')
  })
})

open Mocha
open Test_utils

// Raw code that ends with a line comment is followed by a line break, so that
// the comment does not comment out what is printed after it. A "//" that is
// not a comment adds none.

let add = (a, b) => a + b

let succ: int => int = %raw(`x => x + 1 // add one`)
let one: int = %raw(`1 // one`)
let two = add(%raw(`1 // one`), 1)
let three = (%raw(`y => y + 2 // add two`): int => int)(1)

let lines: array<int> = %raw(`[1, // one
  2]`)

let url: string = %raw(`"http://example.com"`)
let slashes: RegExp.t = %raw(`/\/\//`)

describe(__MODULE__, () => {
  test("raw function ending with a line comment", () => eq(__LOC__, succ(1), 2))

  test("raw literal ending with a line comment", () => {
    eq(__LOC__, one, 1)
    eq(__LOC__, two, 2)
  })

  test("called raw function ending with a line comment", () => eq(__LOC__, three, 3))

  test("line comment before the last line", () => eq(__LOC__, lines, [1, 2]))

  test("slashes that are not a comment", () => {
    eq(__LOC__, url, "http://example.com")
    eq(__LOC__, slashes->RegExp.test("//"), true)
  })
})

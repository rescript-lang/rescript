open Mocha
open Test_utils

let classify = x =>
  switch x {
  | -5 .. -1 => "negative"
  | 0 => "zero"
  | 1 .. 9 => "digit"
  | 0x10 .. 0x1F => "hex"
  | 200 .. 100 => "reversed"
  | _ => "other"
  }

// Bounds may mix chars and ints, as `char` is an alias of `int`.
let ascii = x =>
  switch x {
  | 'a' .. 122 => "lower"
  | 65 .. 'Z' => "upper"
  | _ => "other"
  }

let large = x =>
  switch x {
  | -0x80000000 .. -1 => "negative"
  | 0 .. 0x3FFFFFFF => "low"
  | _ => "high"
  }

let status = code =>
  switch code {
  | 200 .. 299 => "success"
  | 301 | 302 | 307 | 308 => "redirect"
  | 400 .. 499 => "client error"
  | 500 .. 599 => "server error"
  | _ => "unknown"
  }

let pair = (a, b) =>
  switch (a, b) {
  | (0 .. 9, 0 .. 9) => "both"
  | (0 .. 9, _) | (_, 0 .. 9) => "one"
  | _ => "none"
  }

describe(__MODULE__, () => {
  test("ranges", () => {
    eq(__LOC__, classify(-5), "negative")
    eq(__LOC__, classify(-1), "negative")
    eq(__LOC__, classify(-6), "other")
    eq(__LOC__, classify(0), "zero")
    eq(__LOC__, classify(9), "digit")
    eq(__LOC__, classify(16), "hex")
    eq(__LOC__, classify(31), "hex")
    eq(__LOC__, classify(32), "other")
    eq(__LOC__, classify(150), "reversed")
  })

  test("mixed bounds", () => {
    eq(__LOC__, ascii('q'), "lower")
    eq(__LOC__, ascii(81), "upper")
    eq(__LOC__, ascii('!'), "other")
  })

  test("large ranges", () => {
    eq(__LOC__, large(-0x80000000), "negative")
    eq(__LOC__, large(0), "low")
    eq(__LOC__, large(0x3FFFFFFF), "low")
    eq(__LOC__, large(0x40000000), "high")
    eq(__LOC__, large(0x7FFFFFFF), "high")
  })

  test("status codes", () => {
    eq(__LOC__, status(204), "success")
    eq(__LOC__, status(302), "redirect")
    eq(__LOC__, status(304), "unknown")
    eq(__LOC__, status(404), "client error")
    eq(__LOC__, status(503), "server error")
    eq(__LOC__, status(600), "unknown")
  })

  test("several columns", () => {
    eq(__LOC__, pair(1, 2), "both")
    eq(__LOC__, pair(1, 20), "one")
    eq(__LOC__, pair(20, 2), "one")
    eq(__LOC__, pair(20, 20), "none")
  })
})

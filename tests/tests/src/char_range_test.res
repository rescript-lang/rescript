open Mocha
open Test_utils

// A range is one pattern, however many characters it covers, so this one
// compiles as fast as a single character.
let isCjk = c =>
  switch c {
  | '\u{4E00}' .. '\u{9FFF}' => true
  | _ => false
  }

let isCodePoint = c =>
  switch c {
  | '\u{0}' .. '\u{10FFFF}' => true
  | _ => false
  }

let classify = c =>
  switch c {
  | 'a' .. 'z' => "lower"
  | 'A' .. 'Z' => "upper"
  | '0' .. '9' | '_' => "word"
  | '\u{100}' .. '\u{FFFF}' => "bmp"
  | _ => "other"
  }

// Overlapping ranges: the first one that matches wins.
let overlap = c =>
  switch c {
  | 'e' => "e"
  | 'a' .. 'm' => "a-m"
  | 'h' .. 'z' => "h-z"
  | _ => "other"
  }

let guarded = (c, allow) =>
  switch c {
  | 'a' .. 'z' if allow => "allowed"
  | 'a' .. 'm' => "a-m"
  | _ => "other"
  }

// Ranges in several columns
let grid = (a, b) =>
  switch (a, b) {
  | ('a' .. 'c', 'x' .. 'z') => 1
  | ('b' .. 'd', _) => 2
  | (_, 'a' .. 'b' | 'y') => 3
  | ('\u{10000}' .. '\u{10FFFF}', '\u{10000}' .. '\u{10FFFF}') => 4
  | _ => 5
  }

let option = c =>
  switch c {
  | Some('0' .. '9') => "digit"
  | Some(_) => "other"
  | None => "none"
  }

describe(__MODULE__, () => {
  test("large range", () => {
    eq(__LOC__, isCjk('\u{4DFF}'), false)
    eq(__LOC__, isCjk('\u{4E00}'), true)
    eq(__LOC__, isCjk('中'), true)
    eq(__LOC__, isCjk('\u{9FFF}'), true)
    eq(__LOC__, isCjk('\u{A000}'), false)
    eq(__LOC__, isCodePoint('\u{0}'), true)
    eq(__LOC__, isCodePoint('😀'), true)
    eq(__LOC__, isCodePoint('\u{10FFFF}'), true)
    eq(__LOC__, isCodePoint(0x110000), false)
    eq(__LOC__, isCodePoint(-1), false)
  })

  test("several ranges", () => {
    eq(__LOC__, classify('q'), "lower")
    eq(__LOC__, classify('Q'), "upper")
    eq(__LOC__, classify('7'), "word")
    eq(__LOC__, classify('_'), "word")
    eq(__LOC__, classify('ō'), "bmp")
    eq(__LOC__, classify('\u{FFFF}'), "bmp")
    eq(__LOC__, classify('\u{10000}'), "other")
    eq(__LOC__, classify('!'), "other")
  })

  test("overlapping ranges", () => {
    eq(__LOC__, overlap('a'), "a-m")
    eq(__LOC__, overlap('e'), "e")
    eq(__LOC__, overlap('h'), "a-m")
    eq(__LOC__, overlap('m'), "a-m")
    eq(__LOC__, overlap('n'), "h-z")
    eq(__LOC__, overlap('z'), "h-z")
    eq(__LOC__, overlap('{'), "other")
  })

  test("guards", () => {
    eq(__LOC__, guarded('c', true), "allowed")
    eq(__LOC__, guarded('c', false), "a-m")
    eq(__LOC__, guarded('q', false), "other")
    eq(__LOC__, guarded('Q', true), "other")
  })

  test("several columns", () => {
    eq(__LOC__, grid('a', 'y'), 1)
    eq(__LOC__, grid('c', 'z'), 1)
    eq(__LOC__, grid('d', 'z'), 2)
    eq(__LOC__, grid('b', 'b'), 2)
    eq(__LOC__, grid('e', 'b'), 3)
    eq(__LOC__, grid('e', 'y'), 3)
    eq(__LOC__, grid('😀', '😀'), 4)
    eq(__LOC__, grid('😀', 'q'), 5)
    eq(__LOC__, grid('e', 'q'), 5)
  })

  test("option", () => {
    eq(__LOC__, option(Some('5')), "digit")
    eq(__LOC__, option(Some('x')), "other")
    eq(__LOC__, option(None), "none")
  })
})

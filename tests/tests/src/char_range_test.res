open Mocha
open Test_utils

// A range is expanded into one alternative per character. This one has about
// 21,000, which used to take a minute and many GB of memory to compile.
let isCjk = c =>
  switch c {
  | '\u{4E00}' .. '\u{9FFF}' => true
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

describe(__MODULE__, () => {
  test("large range", () => {
    eq(__LOC__, isCjk('\u{4DFF}'), false)
    eq(__LOC__, isCjk('\u{4E00}'), true)
    eq(__LOC__, isCjk('中'), true)
    eq(__LOC__, isCjk('\u{9FFF}'), true)
    eq(__LOC__, isCjk('\u{A000}'), false)
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
})

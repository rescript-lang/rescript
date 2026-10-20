open Mocha
open Test_utils

// `char` is an alias of `int`, so char literals and ints mix freely.
let fromInt: char = 65
let toInt: int = 'a' + 1

let mixed = x =>
  switch x {
  | 'a' => "a"
  | 98 => "b"
  | 'c' .. 'e' => "c-e"
  | 102 | 'g' => "f-g"
  | _ => "other"
  }

// A char range matches the int result of `codePointAt` directly.
let isDigitAt = (s, i) =>
  switch String.codePointAt(s, i) {
  | Some('0' .. '9') => true
  | Some(_) | None => false
  }

// Emoji are outside the BMP: a literal is the full code point, as returned by
// `codePointAt`, not a UTF-16 surrogate.
let emoji = x =>
  switch x {
  | '😀' => "grinning"
  | '\u{1F601}' .. '\u{1F64F}' => "face"
  | 0x1F44B => "wave"
  | 0xD83D => "high surrogate"
  | _ => "other"
  }

let emojiAt = (s, i) =>
  switch String.codePointAt(s, i) {
  | Some(c) => emoji(c)
  | None => "none"
  }

// A char can now hold any int, so the lower bound must be checked.
let isControl = (c: char) =>
  switch c {
  | '\000' .. '\031' => true
  | _ => false
  }

describe(__MODULE__, () => {
  test("conversions", () => {
    eq(__LOC__, fromInt, 'A')
    eq(__LOC__, toInt, 98)
  })

  test("mixed patterns", () => {
    eq(__LOC__, mixed(97), "a")
    eq(__LOC__, mixed('b'), "b")
    eq(__LOC__, mixed('d'), "c-e")
    eq(__LOC__, mixed('f'), "f-g")
    eq(__LOC__, mixed(103), "f-g")
    eq(__LOC__, mixed(104), "other")
    eq(__LOC__, mixed(-97), "other")
  })

  test("code points", () => {
    eq(__LOC__, isDigitAt("a1", 0), false)
    eq(__LOC__, isDigitAt("a1", 1), true)
    eq(__LOC__, isDigitAt("a1", 2), false)
  })

  test("emoji", () => {
    eq(__LOC__, '😀', 0x1F600)
    eq(__LOC__, String.codePointAt("😀", 0), Some('😀'))
    eq(__LOC__, emoji('😀'), "grinning")
    eq(__LOC__, emoji('😂'), "face")
    eq(__LOC__, emoji('👋'), "wave")
    eq(__LOC__, emoji('🚀'), "other")
    eq(__LOC__, emojiAt("a😀", 1), "grinning")
    // Index 2 is the low surrogate of 😀. A lone high surrogate is its own
    // code point.
    eq(__LOC__, emojiAt("a😀", 2), "other")
    eq(__LOC__, emojiAt(String.fromCharCode(0xD83D), 0), "high surrogate")
    eq(__LOC__, emojiAt("a😀", 3), "none")
  })

  test("negative values", () => {
    eq(__LOC__, isControl('\n'), true)
    eq(__LOC__, isControl(' '), false)
    eq(__LOC__, isControl(-1), false)
  })
})

// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

// Below is all deprecated and should be removed in v13

type t = char

external code: t => int = "%identity"

external unsafe_chr: int => t = "%identity"

external chr: int => t = "%identity"

external bytes_create: int => array<char> = "Array"

external bytes_unsafe_set: (array<'a>, int, 'a) => unit = "%array_unsafe_set"

@scope("String") @variadic
external unsafe_to_string: array<char> => string = "fromCodePoint"

let escaped = param =>
  switch param {
  | '\'' => "\\'"
  | '\\' => "\\\\"
  | '\n' => "\\n"
  | '\t' => "\\t"
  | '\r' => "\\r"
  | '\b' => "\\b"
  | ' ' .. '~' as c =>
    let s = bytes_create(1)
    bytes_unsafe_set(s, 0, c)
    unsafe_to_string(s)
  | c =>
    let n = code(c)
    let s = bytes_create(4)
    bytes_unsafe_set(s, 0, '\\')
    bytes_unsafe_set(s, 1, unsafe_chr(48 + n / 100))
    bytes_unsafe_set(s, 2, unsafe_chr(48 + mod(n / 10, 10)))
    bytes_unsafe_set(s, 3, unsafe_chr(48 + mod(n, 10)))
    unsafe_to_string(s)
  }

let lowercase_ascii = c =>
  if c >= 'A' && c <= 'Z' {
    unsafe_chr(code(c) + 32)
  } else {
    c
  }

let uppercase_ascii = c =>
  if c >= 'a' && c <= 'z' {
    unsafe_chr(code(c) - 32)
  } else {
    c
  }

let compare = (c1, c2) => code(c1) - code(c2)
let equal = (c1: t, c2: t) => compare(c1, c2) == 0

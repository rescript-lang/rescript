type t = char

external code: t => int = "%identity"

external unsafe_chr: int => t = "%identity"

external chr: int => t = "%identity"

external fromIntUnsafe: int => t = "%identity"

let fromIntExn = n =>
  if n < 0 || n > 255 {
    throw(Invalid_argument("`Char.fromIntExn` expects an integer between 0 and 255"))
  } else {
    fromIntUnsafe(n)
  }

let fromInt = n =>
  if n < 0 || n > 255 {
    None
  } else {
    Some(fromIntUnsafe(n))
  }

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

let toLowerCaseAscii = c =>
  if c >= 'A' && c <= 'Z' {
    unsafe_chr(code(c) + 32)
  } else {
    c
  }

let lowercase_ascii = toLowerCaseAscii

let toUpperCaseAscii = c =>
  if c >= 'a' && c <= 'z' {
    unsafe_chr(code(c) - 32)
  } else {
    c
  }

let uppercase_ascii = toUpperCaseAscii

external equal: (char, char) => bool = "%equal"
external compare: (char, char) => Stdlib_Ordering.t = "%compare"

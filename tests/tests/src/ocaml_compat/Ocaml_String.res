// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

// Below is all deprecated and should be removed in v13

module Array = Ocaml_Array

type t = string

module B = {
  include Array

  let uppercase_ascii = bytes => map(Char.uppercase_ascii, bytes)
  let lowercase_ascii = bytes => map(Char.lowercase_ascii, bytes)

  let apply1 = (f, bytes) =>
    if length(bytes) == 0 {
      bytes
    } else {
      let r = copy(bytes)
      unsafe_set(r, 0, f(unsafe_get(bytes, 0)))
      r
    }
  let capitalize_ascii = bytes => apply1(Char.uppercase_ascii, bytes)
  let uncapitalize_ascii = bytes => apply1(Char.lowercase_ascii, bytes)

  let escaped = bytes => map(Char.escaped, bytes)
}

@send external join: (array<string>, string) => string = "join"

let concat = (sep: string, xs: list<string>) => xs->Array.of_list->join(sep)

external length: string => int = "%string_length"

@send external get: (string, int) => char = "codePointAt"

@send external unsafe_get: (string, int) => char = "codePointAt"

@scope("Array") external bos: string => array<string> = "from"
let bos = str => B.map(str => str->unsafe_get(0), str->bos)

@scope("String") @variadic
external bts: array<char> => string = "fromCodePoint"

let make = (len, ch) => Primitive_string_extern.fromChar(ch)->Primitive_string_extern.repeat(len)

let init = (len, f) => Array.init(len, i => Primitive_string_extern.fromChar(f(i)))->join("")

let sub = (s, ofs, len) => bts(B.sub(bos(s), ofs, len))

external compare: (t, t) => int = "%compare"

external equal: (t, t) => bool = "%equal"

let iter = (f, s) =>
  for i in 0 to length(s) - 1 {
    f(unsafe_get(s, i))
  }

let iteri = (f, s) =>
  for i in 0 to length(s) - 1 {
    f(i, unsafe_get(s, i))
  }

let map = (f, s) => bts(B.map(f, bos(s)))
let mapi = (f, s) => bts(B.mapi(f, bos(s)))

@send external trim: string => string = "trim"

let escaped = s => {
  let rec needs_escape = i =>
    if i >= length(s) {
      false
    } else {
      switch unsafe_get(s, i) {
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' => true
      | ' ' .. '~' => needs_escape(i + 1)
      | _ => true
      }
    }

  if needs_escape(0) {
    join(B.escaped(bos(s)), "")
  } else {
    s
  }
}

/* duplicated in bytes.ml */
let rec index_rec = (s, lim, i, c) =>
  if i >= lim {
    throw(Not_found)
  } else if unsafe_get(s, i) == c {
    i
  } else {
    index_rec(s, lim, i + 1, c)
  }

/* duplicated in bytes.ml */
let index = (s, c) => index_rec(s, length(s), 0, c)

/* duplicated in bytes.ml */
let rec index_rec_opt = (s, lim, i, c) =>
  if i >= lim {
    None
  } else if unsafe_get(s, i) == c {
    Some(i)
  } else {
    index_rec_opt(s, lim, i + 1, c)
  }

/* duplicated in bytes.ml */
let index_opt = (s, c) => index_rec_opt(s, length(s), 0, c)

/* duplicated in bytes.ml */
let index_from = (s, i, c) => {
  let l = length(s)
  if i < 0 || i > l {
    invalid_arg("String.index_from / Bytes.index_from")
  } else {
    index_rec(s, l, i, c)
  }
}

/* duplicated in bytes.ml */
let index_from_opt = (s, i, c) => {
  let l = length(s)
  if i < 0 || i > l {
    invalid_arg("String.index_from_opt / Bytes.index_from_opt")
  } else {
    index_rec_opt(s, l, i, c)
  }
}

/* duplicated in bytes.ml */
let rec rindex_rec = (s, i, c) =>
  if i < 0 {
    throw(Not_found)
  } else if unsafe_get(s, i) == c {
    i
  } else {
    rindex_rec(s, i - 1, c)
  }

/* duplicated in bytes.ml */
let rindex = (s, c) => rindex_rec(s, length(s) - 1, c)

/* duplicated in bytes.ml */
let rindex_from = (s, i, c) =>
  if i < -1 || i >= length(s) {
    invalid_arg("String.rindex_from / Bytes.rindex_from")
  } else {
    rindex_rec(s, i, c)
  }

/* duplicated in bytes.ml */
let rec rindex_rec_opt = (s, i, c) =>
  if i < 0 {
    None
  } else if unsafe_get(s, i) == c {
    Some(i)
  } else {
    rindex_rec_opt(s, i - 1, c)
  }

/* duplicated in bytes.ml */
let rindex_opt = (s, c) => rindex_rec_opt(s, length(s) - 1, c)

/* duplicated in bytes.ml */
let rindex_from_opt = (s, i, c) =>
  if i < -1 || i >= length(s) {
    invalid_arg("String.rindex_from_opt / Bytes.rindex_from_opt")
  } else {
    rindex_rec_opt(s, i, c)
  }

/* duplicated in bytes.ml */
let contains_from = (s, i, c) => {
  let l = length(s)
  if i < 0 || i > l {
    invalid_arg("String.contains_from / Bytes.contains_from")
  } else {
    try {
      ignore(index_rec(s, l, i, c))
      true
    } catch {
    | Not_found => false
    }
  }
}

/* duplicated in bytes.ml */
let contains = (s, c) => contains_from(s, 0, c)

/* duplicated in bytes.ml */
let rcontains_from = (s, i, c) =>
  if i < 0 || i >= length(s) {
    invalid_arg("String.rcontains_from / Bytes.rcontains_from")
  } else {
    try {
      ignore(rindex_rec(s, i, c))
      true
    } catch {
    | Not_found => false
    }
  }

let uppercase_ascii = s => bts(B.uppercase_ascii(bos(s)))
let lowercase_ascii = s => bts(B.lowercase_ascii(bos(s)))
let capitalize_ascii = s => bts(B.capitalize_ascii(bos(s)))
let uncapitalize_ascii = s => bts(B.uncapitalize_ascii(bos(s)))

let split_on_char = (sep, s) => {
  let r = ref(list{})
  let j = ref(length(s))
  for i in length(s) - 1 downto 0 {
    if unsafe_get(s, i) == sep {
      r := list{sub(s, i + 1, j.contents - i - 1), ...r.contents}
      j := i
    }
  }
  list{sub(s, 0, j.contents), ...r.contents}
}

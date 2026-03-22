// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Point-to-point test: Lexer/Scanner — token recognition and literal parsing
// Tests that every token category is correctly lexed and produces correct
// runtime values after full compilation.

let eq = (a, b) => a == b
let neq = (a, b) => a != b
let approxEq = (a, b) => Float.Math.abs(a -. b) < 0.0001

// ─── Integer literals ───────────────────────────────────────────────
Test.run(__POS_OF__("int: decimal zero"), 0, eq, 0)
Test.run(__POS_OF__("int: positive decimal"), 42, eq, 42)
Test.run(__POS_OF__("int: negative decimal"), -1, eq, -1)
Test.run(__POS_OF__("int: large positive"), 2_147_483_647, eq, 2147483647)
Test.run(__POS_OF__("int: underscored"), 1_000_000, eq, 1000000)
Test.run(__POS_OF__("int: hex lowercase"), 0xff, eq, 255)
Test.run(__POS_OF__("int: hex uppercase"), 0xFF, eq, 255)
Test.run(__POS_OF__("int: hex underscored"), 0xFF_FF, eq, 65535)
Test.run(__POS_OF__("int: octal"), 0o77, eq, 63)
Test.run(__POS_OF__("int: binary"), 0b1010, eq, 10)
Test.run(__POS_OF__("int: binary underscored"), 0b1111_0000, eq, 240)

// ─── Float literals ─────────────────────────────────────────────────
Test.run(__POS_OF__("float: simple"), 3.14, approxEq, 3.14)
Test.run(__POS_OF__("float: no fractional"), 42.0, approxEq, 42.0)
Test.run(__POS_OF__("float: leading dot"), 0.5, approxEq, 0.5)
Test.run(__POS_OF__("float: exponent lowercase"), 1e3, approxEq, 1000.0)
Test.run(__POS_OF__("float: exponent uppercase"), 1E3, approxEq, 1000.0)
Test.run(__POS_OF__("float: negative exponent"), 1e-2, approxEq, 0.01)
Test.run(__POS_OF__("float: positive exponent"), 1e+2, approxEq, 100.0)
Test.run(__POS_OF__("float: combined"), 6.022e23, approxEq, 6.022e23)
Test.run(__POS_OF__("float: negative"), -273.15, approxEq, -273.15)
Test.run(__POS_OF__("float: infinity"), infinity, eq, infinity)
Test.run(__POS_OF__("float: neg_infinity"), neg_infinity, eq, neg_infinity)
Test.run(__POS_OF__("float: nan is nan"), Float.isNaN(nan), eq, true)

// ─── String literals ────────────────────────────────────────────────
Test.run(__POS_OF__("string: empty"), "", eq, "")
Test.run(__POS_OF__("string: simple"), "hello", eq, "hello")
Test.run(__POS_OF__("string: with spaces"), "hello world", eq, "hello world")
Test.run(__POS_OF__("string: escape newline"), "a\nb", eq, "a\nb")
Test.run(__POS_OF__("string: escape tab"), "a\tb", eq, "a\tb")
Test.run(__POS_OF__("string: escape backslash"), "a\\b", eq, "a\\b")
Test.run(__POS_OF__("string: escape quote"), "a\"b", eq, "a\"b")
Test.run(__POS_OF__("string: unicode escape"), "\u0041", eq, "A")
Test.run(__POS_OF__("string: unicode braced"), "\u{1F600}", eq, "\u{1F600}")
Test.run(__POS_OF__("string: null char"), "\x00", eq, "\x00")
Test.run(__POS_OF__("string: hex escape"), "\x41", eq, "A")

// String interpolation
let name = "world"
Test.run(__POS_OF__("string: interpolation simple"), `hello ${name}`, eq, "hello world")
let x = 42
Test.run(__POS_OF__("string: interpolation expr"), `value is ${Int.toString(x)}`, eq, "value is 42")
Test.run(__POS_OF__("string: interpolation nested"), `a${`b${"c"}`}d`, eq, "abcd")
Test.run(__POS_OF__("string: interpolation empty"), `${""}`, eq, "")

// Multiline strings
let multiline = "line1
line2
line3"
Test.run(__POS_OF__("string: multiline"), String.includes(multiline, "\n"), eq, true)
Test.run(
  __POS_OF__("string: multiline line count"),
  Array.length(String.split(multiline, "\n")),
  eq,
  3,
)

// ─── Char literals ──────────────────────────────────────────────────
Test.run(__POS_OF__("char: simple"), 'A', eq, 'A')
Test.run(__POS_OF__("char: digit"), '0', eq, '0')
Test.run(__POS_OF__("char: space"), ' ', eq, ' ')
Test.run(__POS_OF__("char: to int"), Char.toInt('A'), eq, 65)
Test.run(__POS_OF__("char: from int"), Char.fromIntExn(65), eq, 'A')

// ─── Boolean literals ───────────────────────────────────────────────
Test.run(__POS_OF__("bool: true"), true, eq, true)
Test.run(__POS_OF__("bool: false"), false, eq, false)
Test.run(__POS_OF__("bool: negation"), !true, eq, false)
Test.run(__POS_OF__("bool: double negation"), !!true, eq, true)

// ─── Unit literal ───────────────────────────────────────────────────
Test.run(__POS_OF__("unit: value"), (), eq, ())

// ─── Arithmetic operators ───────────────────────────────────────────
Test.run(__POS_OF__("op: int add"), 2 + 3, eq, 5)
Test.run(__POS_OF__("op: int sub"), 10 - 3, eq, 7)
Test.run(__POS_OF__("op: int mul"), 4 * 5, eq, 20)
Test.run(__POS_OF__("op: int div"), 10 / 3, eq, 3)
Test.run(__POS_OF__("op: int mod"), 10 mod 3, eq, 1)
Test.run(__POS_OF__("op: float add"), 1.5 +. 2.5, approxEq, 4.0)
Test.run(__POS_OF__("op: float sub"), 5.0 -. 2.0, approxEq, 3.0)
Test.run(__POS_OF__("op: float mul"), 2.0 *. 3.0, approxEq, 6.0)
Test.run(__POS_OF__("op: float div"), 10.0 /. 4.0, approxEq, 2.5)
Test.run(__POS_OF__("op: unary minus int"), -(5), eq, -5)
Test.run(__POS_OF__("op: unary minus float"), -.(5.0), approxEq, -5.0)

// ─── Comparison operators ───────────────────────────────────────────
Test.run(__POS_OF__("op: equal"), 1 == 1, eq, true)
Test.run(__POS_OF__("op: not equal"), 1 != 2, eq, true)
Test.run(__POS_OF__("op: structural eq"), [1, 2] == [1, 2], eq, true)
Test.run(__POS_OF__("op: referential eq"), 1 === 1, eq, true)
Test.run(__POS_OF__("op: less than"), 1 < 2, eq, true)
Test.run(__POS_OF__("op: less equal"), 1 <= 1, eq, true)
Test.run(__POS_OF__("op: greater"), 2 > 1, eq, true)
Test.run(__POS_OF__("op: greater equal"), 2 >= 2, eq, true)

// ─── Logical operators ─────────────────────────────────────────────
Test.run(__POS_OF__("op: logical and true"), true && true, eq, true)
Test.run(__POS_OF__("op: logical and false"), true && false, eq, false)
Test.run(__POS_OF__("op: logical or false"), false || false, eq, false)
Test.run(__POS_OF__("op: logical or true"), false || true, eq, true)
Test.run(__POS_OF__("op: logical not"), !true, eq, false)

// Short-circuit evaluation
let sideEffect = ref(0)
let _ = false && {
  sideEffect := 1
  true
}
Test.run(__POS_OF__("op: and short-circuits"), sideEffect.contents, eq, 0)

let _ = true || {
  sideEffect := 2
  false
}
Test.run(__POS_OF__("op: or short-circuits"), sideEffect.contents, eq, 0)

// ─── String operators ───────────────────────────────────────────────
Test.run(__POS_OF__("op: string concat"), "hello" ++ " " ++ "world", eq, "hello world")
Test.run(__POS_OF__("op: string concat empty"), "" ++ "a" ++ "", eq, "a")

// ─── Pipe operator ──────────────────────────────────────────────────
Test.run(__POS_OF__("op: pipe"), 5->Int.toString, eq, "5")
Test.run(__POS_OF__("op: pipe chain"), 42->Int.toString->String.length, eq, 2)
Test.run(
  __POS_OF__("op: pipe with array"),
  [1, 2, 3]->Array.map(x => x * 2)->Array.reduce(0, (acc, x) => acc + x),
  eq,
  12,
)

// ─── Array literals ─────────────────────────────────────────────────
Test.run(__POS_OF__("array: empty"), [], eq, [])
Test.run(__POS_OF__("array: single"), [1], eq, [1])
Test.run(__POS_OF__("array: multiple"), [1, 2, 3], eq, [1, 2, 3])
Test.run(__POS_OF__("array: nested"), [[1, 2], [3, 4]], eq, [[1, 2], [3, 4]])
Test.run(__POS_OF__("array: mixed types via variant"), [#int(1), #str("a")], eq, [#int(1), #str("a")])

// ─── Tuple literals ─────────────────────────────────────────────────
let (a, b) = (1, 2)
Test.run(__POS_OF__("tuple: fst"), a, eq, 1)
Test.run(__POS_OF__("tuple: snd"), b, eq, 2)
let (c, d, e) = (true, "hi", 3.14)
Test.run(__POS_OF__("tuple: triple bool"), c, eq, true)
Test.run(__POS_OF__("tuple: triple string"), d, eq, "hi")
Test.run(__POS_OF__("tuple: triple float"), e, approxEq, 3.14)

// ─── Record literals ────────────────────────────────────────────────
type point = {x: int, y: int}
let p = {x: 10, y: 20}
Test.run(__POS_OF__("record: field x"), p.x, eq, 10)
Test.run(__POS_OF__("record: field y"), p.y, eq, 20)
let p2 = {...p, y: 30}
Test.run(__POS_OF__("record: spread x unchanged"), p2.x, eq, 10)
Test.run(__POS_OF__("record: spread y updated"), p2.y, eq, 30)

// ─── Variant constructors ───────────────────────────────────────────
type color = Red | Green | Blue | Custom(int, int, int)
Test.run(__POS_OF__("variant: simple eq"), Red, eq, Red)
Test.run(__POS_OF__("variant: simple neq"), Red, neq, Green)
Test.run(
  __POS_OF__("variant: with payload"),
  switch Custom(255, 0, 0) {
  | Custom(r, _, _) => r
  | _ => -1
  },
  eq,
  255,
)

// ─── Polymorphic variant literals ───────────────────────────────────
Test.run(__POS_OF__("polyvar: simple"), #red, eq, #red)
Test.run(__POS_OF__("polyvar: with payload"), #rgb(1, 2, 3), eq, #rgb(1, 2, 3))
Test.run(
  __POS_OF__("polyvar: switch"),
  switch #hello {
  | #hello => "yes"
  | #world => "no"
  },
  eq,
  "yes",
)

// ─── BigInt literals ────────────────────────────────────────────────
Test.run(__POS_OF__("bigint: simple"), 42n, eq, 42n)
Test.run(__POS_OF__("bigint: zero"), 0n, eq, 0n)
Test.run(__POS_OF__("bigint: large"), 9007199254740993n, eq, 9007199254740993n)
Test.run(__POS_OF__("bigint: negative"), -1n, eq, -1n)
Test.run(__POS_OF__("bigint: add"), 1n + 2n, eq, 3n)
Test.run(__POS_OF__("bigint: mul"), 3n * 4n, eq, 12n)

// ─── Identifier forms ──────────────────────────────────────────────
let _underscore = 1
Test.run(__POS_OF__("ident: underscore prefix"), _underscore, eq, 1)
let camelCase = 2
Test.run(__POS_OF__("ident: camelCase"), camelCase, eq, 2)
let x' = 3
Test.run(__POS_OF__("ident: primed"), x', eq, 3)
let x'' = 4
Test.run(__POS_OF__("ident: double primed"), x'', eq, 4)

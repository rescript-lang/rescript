// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Point-to-point test: JS Code Generation — expression compilation
// Tests that ReScript expressions compile to correct JavaScript and produce
// correct runtime values.

let eq = (a, b) => a == b
let approxEq = (a, b) => Float.Math.abs(a -. b) < 0.0001

// ─── Arithmetic compilation ─────────────────────────────────────────
Test.run(__POS_OF__("js arith: add"), 2 + 3, eq, 5)
Test.run(__POS_OF__("js arith: sub"), 10 - 7, eq, 3)
Test.run(__POS_OF__("js arith: mul"), 6 * 7, eq, 42)
Test.run(__POS_OF__("js arith: div"), 15 / 4, eq, 3) // integer division
Test.run(__POS_OF__("js arith: mod"), 17 mod 5, eq, 2)
Test.run(__POS_OF__("js arith: neg"), -(42), eq, -42)
Test.run(__POS_OF__("js arith: float add"), 1.1 +. 2.2, approxEq, 3.3)
Test.run(__POS_OF__("js arith: float mul"), 2.5 *. 4.0, approxEq, 10.0)
Test.run(__POS_OF__("js arith: float div"), 10.0 /. 3.0, approxEq, 3.333333)
Test.run(__POS_OF__("js arith: precedence"), 2 + 3 * 4, eq, 14)
Test.run(__POS_OF__("js arith: parens"), (2 + 3) * 4, eq, 20)

// ─── String operations ─────────────────────────────────────────────
Test.run(__POS_OF__("js str: concat"), "a" ++ "b" ++ "c", eq, "abc")
Test.run(__POS_OF__("js str: length"), String.length("hello"), eq, 5)
Test.run(__POS_OF__("js str: charAt"), String.charAt("hello", 1), eq, "e")
Test.run(
  __POS_OF__("js str: includes"),
  String.includes("hello world", "world"),
  eq,
  true,
)
Test.run(
  __POS_OF__("js str: startsWith"),
  String.startsWith("hello", "hel"),
  eq,
  true,
)
Test.run(
  __POS_OF__("js str: endsWith"),
  String.endsWith("hello", "llo"),
  eq,
  true,
)
Test.run(
  __POS_OF__("js str: slice"),
  String.slice("hello world", ~start=6, ~end=11),
  eq,
  "world",
)
Test.run(__POS_OF__("js str: trim"), String.trim("  hello  "), eq, "hello")
Test.run(
  __POS_OF__("js str: split"),
  String.split("a,b,c", ","),
  eq,
  ["a", "b", "c"],
)
Test.run(
  __POS_OF__("js str: toLowerCase"),
  String.toLowerCase("HELLO"),
  eq,
  "hello",
)
Test.run(
  __POS_OF__("js str: toUpperCase"),
  String.toUpperCase("hello"),
  eq,
  "HELLO",
)
Test.run(
  __POS_OF__("js str: replaceAll"),
  String.replaceAll("aaa", "a", "b"),
  eq,
  "bbb",
)

// String interpolation
let who = "world"
Test.run(__POS_OF__("js str: interpolation"), `hello ${who}`, eq, "hello world")

// ─── Array operations ───────────────────────────────────────────────
let arr = [1, 2, 3, 4, 5]
Test.run(__POS_OF__("js arr: length"), Array.length(arr), eq, 5)
Test.run(__POS_OF__("js arr: get"), arr[2], eq, 3)
Test.run(__POS_OF__("js arr: map"), [1, 2, 3]->Array.map(x => x * 2), eq, [2, 4, 6])
Test.run(
  __POS_OF__("js arr: filter"),
  [1, 2, 3, 4, 5]->Array.filter(x => x mod 2 == 0),
  eq,
  [2, 4],
)
Test.run(
  __POS_OF__("js arr: reduce"),
  [1, 2, 3, 4]->Array.reduce(0, (acc, x) => acc + x),
  eq,
  10,
)
Test.run(__POS_OF__("js arr: some"), [1, 2, 3]->Array.some(x => x > 2), eq, true)
Test.run(__POS_OF__("js arr: every"), [2, 4, 6]->Array.every(x => x mod 2 == 0), eq, true)
Test.run(__POS_OF__("js arr: find"), [1, 2, 3]->Array.find(x => x > 1), eq, Some(2))
Test.run(
  __POS_OF__("js arr: findIndex"),
  [10, 20, 30]->Array.findIndex(x => x == 20),
  eq,
  1,
)
Test.run(__POS_OF__("js arr: concat"), Array.concat([1, 2], [3, 4]), eq, [1, 2, 3, 4])
Test.run(__POS_OF__("js arr: slice"), [1, 2, 3, 4, 5]->Array.slice(~start=1, ~end=4), eq, [2, 3, 4])
Test.run(
  __POS_OF__("js arr: flat"),
  [[1, 2], [3, 4], [5]]->Array.flat,
  eq,
  [1, 2, 3, 4, 5],
)
Test.run(
  __POS_OF__("js arr: flatMap"),
  [1, 2, 3]->Array.flatMap(x => [x, x * 10]),
  eq,
  [1, 10, 2, 20, 3, 30],
)
Test.run(
  __POS_OF__("js arr: includes"),
  [1, 2, 3]->Array.includes(2),
  eq,
  true,
)
Test.run(
  __POS_OF__("js arr: join"),
  ["a", "b", "c"]->Array.join(", "),
  eq,
  "a, b, c",
)
Test.run(
  __POS_OF__("js arr: reverse"),
  [1, 2, 3]->Array.toReversed,
  eq,
  [3, 2, 1],
)

// ─── Record/object operations ───────────────────────────────────────
type point = {x: int, y: int}
let p = {x: 10, y: 20}
Test.run(__POS_OF__("js obj: field access x"), p.x, eq, 10)
Test.run(__POS_OF__("js obj: field access y"), p.y, eq, 20)

let p2 = {...p, x: 30}
Test.run(__POS_OF__("js obj: spread"), p2.x, eq, 30)
Test.run(__POS_OF__("js obj: spread preserved"), p2.y, eq, 20)

type mutablePoint = {mutable mx: int, mutable my: int}
let mp = {mx: 0, my: 0}
mp.mx = 5
mp.my = 10
Test.run(__POS_OF__("js obj: mutable set x"), mp.mx, eq, 5)
Test.run(__POS_OF__("js obj: mutable set y"), mp.my, eq, 10)

// ─── Pattern matching compilation ───────────────────────────────────
// Simple variant dispatch
type traffic = Red2 | Yellow2 | Green2
let action = light =>
  switch light {
  | Red2 => "stop"
  | Yellow2 => "caution"
  | Green2 => "go"
  }
Test.run(__POS_OF__("js match: variant"), action(Green2), eq, "go")

// Match with guards
let classify = n =>
  switch n {
  | n if n < 0 => "negative"
  | 0 => "zero"
  | n if n < 10 => "small"
  | n if n < 100 => "medium"
  | _ => "large"
  }
Test.run(__POS_OF__("js match: guard neg"), classify(-5), eq, "negative")
Test.run(__POS_OF__("js match: guard zero"), classify(0), eq, "zero")
Test.run(__POS_OF__("js match: guard small"), classify(5), eq, "small")
Test.run(__POS_OF__("js match: guard medium"), classify(50), eq, "medium")
Test.run(__POS_OF__("js match: guard large"), classify(200), eq, "large")

// ─── Curried vs uncurried calling ───────────────────────────────────
let add = (a, b) => a + b
Test.run(__POS_OF__("js call: uncurried"), add(3, 4), eq, 7)

// Partial application
let add5 = add(5, ...)
Test.run(__POS_OF__("js call: partial"), add5(10), eq, 15)

// ─── Exception handling ────────────────────────────────────────────
exception JsCodegenError(string)

let safeParse = s =>
  try {
    Some(Int.fromString(s)->Option.getExn)
  } catch {
  | _ => None
  }
Test.run(__POS_OF__("js try: success"), safeParse("42"), eq, Some(42))
Test.run(__POS_OF__("js try: failure"), safeParse("abc"), eq, None)

// Nested try/catch
let nestedTry = () => {
  try {
    try {
      raise(JsCodegenError("inner"))
    } catch {
    | JsCodegenError(msg) => "caught: " ++ msg
    }
  } catch {
  | _ => "outer"
  }
}
Test.run(__POS_OF__("js try: nested"), nestedTry(), eq, "caught: inner")

// ─── Pipe operator compilation ──────────────────────────────────────
Test.run(
  __POS_OF__("js pipe: chain"),
  42->Int.toString->String.length,
  eq,
  2,
)
Test.run(
  __POS_OF__("js pipe: with map"),
  [1, 2, 3]
  ->Array.map(x => x * 2)
  ->Array.filter(x => x > 2)
  ->Array.reduce(0, (a, b) => a + b),
  eq,
  10,
)

// ─── Module access compilation ──────────────────────────────────────
module MathUtils = {
  let square = x => x * x
  let cube = x => x * x * x
  module Advanced = {
    let power = (base, exp) => {
      let result = ref(1)
      for _ in 1 to exp {
        result := result.contents * base
      }
      result.contents
    }
  }
}

Test.run(__POS_OF__("js module: direct"), MathUtils.square(5), eq, 25)
Test.run(__POS_OF__("js module: cube"), MathUtils.cube(3), eq, 27)
Test.run(__POS_OF__("js module: nested"), MathUtils.Advanced.power(2, 10), eq, 1024)

// ─── For loop compilation ───────────────────────────────────────────
let forResult = {
  let sum = ref(0)
  for i in 1 to 10 {
    sum := sum.contents + i
  }
  sum.contents
}
Test.run(__POS_OF__("js for: sum 1..10"), forResult, eq, 55)

// Downto
let downResult = {
  let arr = []
  for i in 5 downto 1 {
    ignore(arr->Array.push(i))
  }
  arr
}
Test.run(__POS_OF__("js for: downto"), downResult, eq, [5, 4, 3, 2, 1])

// ─── While loop compilation ────────────────────────────────────────
let whileResult = {
  let n = ref(1)
  let count = ref(0)
  while n.contents < 100 {
    n := n.contents * 2
    count := count.contents + 1
  }
  count.contents
}
Test.run(__POS_OF__("js while: power of 2"), whileResult, eq, 7)

// ─── Dict compilation ──────────────────────────────────────────────
let d = Dict.make()
d->Dict.set("a", 1)
d->Dict.set("b", 2)
d->Dict.set("c", 3)
Test.run(__POS_OF__("js dict: get"), d->Dict.get("b"), eq, Some(2))
Test.run(__POS_OF__("js dict: get missing"), d->Dict.get("z"), eq, None)
Test.run(__POS_OF__("js dict: keys length"), Array.length(Dict.keysToArray(d)), eq, 3)

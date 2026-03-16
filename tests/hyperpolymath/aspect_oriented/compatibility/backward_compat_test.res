// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Aspect-oriented test: Backward compatibility
// Cross-cutting concern: existing patterns and APIs must continue to work
// as expected in v13.0.0-alpha.2.

let eq = (a, b) => a == b

// ─── Belt module API compatibility ──────────────────────────────────
// Belt.Array
Test.run(
  __POS_OF__("compat Belt.Array: map"),
  Belt.Array.map([1, 2, 3], x => x * 2),
  eq,
  [2, 4, 6],
)
Test.run(
  __POS_OF__("compat Belt.Array: keep"),
  Belt.Array.keep([1, 2, 3, 4], x => x mod 2 == 0),
  eq,
  [2, 4],
)
Test.run(
  __POS_OF__("compat Belt.Array: reduce"),
  Belt.Array.reduce([1, 2, 3], 0, (a, b) => a + b),
  eq,
  6,
)
Test.run(
  __POS_OF__("compat Belt.Array: length"),
  Belt.Array.length([1, 2, 3]),
  eq,
  3,
)
Test.run(
  __POS_OF__("compat Belt.Array: get"),
  Belt.Array.get([10, 20, 30], 1),
  eq,
  Some(20),
)
Test.run(
  __POS_OF__("compat Belt.Array: getExn"),
  Belt.Array.getExn([10, 20, 30], 2),
  eq,
  30,
)

// Belt.Option
Test.run(
  __POS_OF__("compat Belt.Option: map"),
  Belt.Option.map(Some(5), x => x * 2),
  eq,
  Some(10),
)
Test.run(
  __POS_OF__("compat Belt.Option: getWithDefault"),
  Belt.Option.getWithDefault(None, 42),
  eq,
  42,
)
Test.run(
  __POS_OF__("compat Belt.Option: isSome"),
  Belt.Option.isSome(Some(1)),
  eq,
  true,
)
Test.run(
  __POS_OF__("compat Belt.Option: isNone"),
  Belt.Option.isNone(None),
  eq,
  true,
)
Test.run(
  __POS_OF__("compat Belt.Option: flatMap"),
  Belt.Option.flatMap(Some(5), x => x > 3 ? Some(x) : None),
  eq,
  Some(5),
)

// Belt.Result
Test.run(
  __POS_OF__("compat Belt.Result: map"),
  Belt.Result.map(Ok(5), x => x * 2),
  eq,
  Ok(10),
)
Test.run(
  __POS_OF__("compat Belt.Result: getExn"),
  Belt.Result.getExn(Ok(42)),
  eq,
  42,
)
Test.run(
  __POS_OF__("compat Belt.Result: isOk"),
  Belt.Result.isOk(Ok(1)),
  eq,
  true,
)
Test.run(
  __POS_OF__("compat Belt.Result: isError"),
  Belt.Result.isError(Error("x")),
  eq,
  true,
)

// Belt.Int
Test.run(__POS_OF__("compat Belt.Int: toString"), Belt.Int.toString(42), eq, "42")

// Belt.Float
Test.run(__POS_OF__("compat Belt.Float: toString"), Belt.Float.toString(3.14), eq, "3.14")

// Belt.List
Test.run(
  __POS_OF__("compat Belt.List: toArray"),
  Belt.List.toArray(list{1, 2, 3}),
  eq,
  [1, 2, 3],
)
Test.run(
  __POS_OF__("compat Belt.List: length"),
  Belt.List.length(list{1, 2, 3}),
  eq,
  3,
)
Test.run(
  __POS_OF__("compat Belt.List: map"),
  Belt.List.map(list{1, 2, 3}, x => x * 2)->Belt.List.toArray,
  eq,
  [2, 4, 6],
)

// ─── Js module compatibility ────────────────────────────────────────
Test.run(
  __POS_OF__("compat Js.Array2: length"),
  Js.Array2.length([1, 2, 3]),
  eq,
  3,
)
Test.run(
  __POS_OF__("compat Js.String2: length"),
  Js.String2.length("hello"),
  eq,
  5,
)

// ─── Old-style pattern matching ─────────────────────────────────────
type oldVariant = OldA | OldB(int) | OldC(string, int)

let matchOld = x =>
  switch x {
  | OldA => "a"
  | OldB(n) => `b:${Int.toString(n)}`
  | OldC(s, n) => `c:${s}:${Int.toString(n)}`
  }

Test.run(__POS_OF__("compat: pattern a"), matchOld(OldA), eq, "a")
Test.run(__POS_OF__("compat: pattern b"), matchOld(OldB(42)), eq, "b:42")
Test.run(__POS_OF__("compat: pattern c"), matchOld(OldC("x", 1)), eq, "c:x:1")

// ─── Curried function behavior ──────────────────────────────────────
let curriedAdd = (a, b) => a + b
Test.run(__POS_OF__("compat: curried direct"), curriedAdd(3, 4), eq, 7)

// Partial application
let partialAdd = curriedAdd(10, ...)
Test.run(__POS_OF__("compat: partial apply"), partialAdd(5), eq, 15)

// ─── Legacy record syntax ──────────────────────────────────────────
type legacyRec = {name: string, value: int}
let lr = {name: "test", value: 42}
Test.run(__POS_OF__("compat: record name"), lr.name, eq, "test")
Test.run(__POS_OF__("compat: record value"), lr.value, eq, 42)

// Record update
let lr2 = {...lr, value: 99}
Test.run(__POS_OF__("compat: record update"), lr2.value, eq, 99)
Test.run(__POS_OF__("compat: record update preserves"), lr2.name, eq, "test")

// ─── Array literal compatibility ────────────────────────────────────
Test.run(__POS_OF__("compat: empty array"), [], eq, [])
Test.run(__POS_OF__("compat: int array"), [1, 2, 3], eq, [1, 2, 3])
Test.run(__POS_OF__("compat: string array"), ["a", "b"], eq, ["a", "b"])
Test.run(__POS_OF__("compat: nested array"), [[1], [2, 3]], eq, [[1], [2, 3]])

// ─── List compatibility ─────────────────────────────────────────────
Test.run(
  __POS_OF__("compat: list literal"),
  List.toArray(list{1, 2, 3}),
  eq,
  [1, 2, 3],
)
Test.run(
  __POS_OF__("compat: list cons"),
  List.toArray(list{0, ...list{1, 2, 3}}),
  eq,
  [0, 1, 2, 3],
)
Test.run(
  __POS_OF__("compat: list head"),
  List.head(list{1, 2, 3}),
  eq,
  Some(1),
)

// ─── Option compatibility ──────────────────────────────────────────
Test.run(__POS_OF__("compat: option some"), Some(42), eq, Some(42))
Test.run(__POS_OF__("compat: option none eq"), None == None, eq, true)

// ─── Exception compatibility ────────────────────────────────────────
exception LegacyExn(string)

let legacyExnTest =
  try {
    raise(LegacyExn("test"))
  } catch {
  | LegacyExn(msg) => msg
  | _ => "unknown"
  }
Test.run(__POS_OF__("compat: exception"), legacyExnTest, eq, "test")

// ─── Ref compatibility ─────────────────────────────────────────────
let r = ref(0)
r := 42
Test.run(__POS_OF__("compat: ref set"), r.contents, eq, 42)
r.contents = 99
Test.run(__POS_OF__("compat: ref contents"), r.contents, eq, 99)

// ─── Pipe compatibility ────────────────────────────────────────────
Test.run(
  __POS_OF__("compat: pipe ->"),
  42->Int.toString->String.length,
  eq,
  2,
)
Test.run(
  __POS_OF__("compat: pipe chain"),
  [1, 2, 3]->Array.map(x => x * 2)->Array.reduce(0, (a, b) => a + b),
  eq,
  12,
)

// ─── Labeled argument compatibility ─────────────────────────────────
let withLabels = (~a, ~b, ~c=0) => a + b + c
Test.run(__POS_OF__("compat: labels"), withLabels(~a=1, ~b=2), eq, 3)
Test.run(__POS_OF__("compat: labels with opt"), withLabels(~a=1, ~b=2, ~c=3), eq, 6)

// ─── Polymorphic variant compatibility ──────────────────────────────
let pvTest = (v: [> #a | #b(int)]) =>
  switch v {
  | #a => 0
  | #b(n) => n
  | _ => -1
  }
Test.run(__POS_OF__("compat: polyvar a"), pvTest(#a), eq, 0)
Test.run(__POS_OF__("compat: polyvar b"), pvTest(#b(42)), eq, 42)

// ─── String interpolation compatibility ─────────────────────────────
let x = 42
Test.run(__POS_OF__("compat: interpolation"), `value: ${Int.toString(x)}`, eq, "value: 42")
Test.run(__POS_OF__("compat: interp concat"), `${"a" ++ "b"}`, eq, "ab")

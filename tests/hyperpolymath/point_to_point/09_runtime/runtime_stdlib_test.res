// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Point-to-point test: Runtime — standard library
// Tests the ReScript standard library modules: Array, String, Int, Float,
// Option, Result, Dict, Math, JSON, Promise, RegExp, and more.

let eq = (a, b) => a == b
let approxEq = (a, b) => Float.Math.abs(a -. b) < 0.0001

// ═══════════════════════════════════════════════════════════════════
// Array module
// ═══════════════════════════════════════════════════════════════════
Test.run(__POS_OF__("Array.length empty"), Array.length([]), eq, 0)
Test.run(__POS_OF__("Array.length 3"), Array.length([1, 2, 3]), eq, 3)
Test.run(__POS_OF__("Array.map"), [1, 2, 3]->Array.map(x => x * 2), eq, [2, 4, 6])
Test.run(__POS_OF__("Array.map empty"), []->Array.map(x => x + 1), eq, [])
Test.run(
  __POS_OF__("Array.filter"),
  [1, 2, 3, 4, 5]->Array.filter(x => x mod 2 == 0),
  eq,
  [2, 4],
)
Test.run(
  __POS_OF__("Array.reduce"),
  [1, 2, 3, 4]->Array.reduce(0, (acc, x) => acc + x),
  eq,
  10,
)
Test.run(
  __POS_OF__("Array.reduce empty"),
  []->Array.reduce(42, (acc, _x) => acc),
  eq,
  42,
)
Test.run(__POS_OF__("Array.some true"), [1, 2, 3]->Array.some(x => x == 2), eq, true)
Test.run(__POS_OF__("Array.some false"), [1, 2, 3]->Array.some(x => x == 5), eq, false)
Test.run(__POS_OF__("Array.some empty"), []->Array.some(_x => true), eq, false)
Test.run(
  __POS_OF__("Array.every true"),
  [2, 4, 6]->Array.every(x => x mod 2 == 0),
  eq,
  true,
)
Test.run(
  __POS_OF__("Array.every false"),
  [2, 3, 6]->Array.every(x => x mod 2 == 0),
  eq,
  false,
)
Test.run(__POS_OF__("Array.find some"), [1, 2, 3]->Array.find(x => x > 1), eq, Some(2))
Test.run(__POS_OF__("Array.find none"), [1, 2, 3]->Array.find(x => x > 5), eq, None)
Test.run(
  __POS_OF__("Array.findIndex found"),
  [10, 20, 30]->Array.findIndex(x => x == 20),
  eq,
  1,
)
Test.run(
  __POS_OF__("Array.findIndex not found"),
  [10, 20, 30]->Array.findIndex(x => x == 50),
  eq,
  -1,
)
Test.run(
  __POS_OF__("Array.concat"),
  Array.concat([1, 2], [3, 4]),
  eq,
  [1, 2, 3, 4],
)
Test.run(
  __POS_OF__("Array.slice"),
  [1, 2, 3, 4, 5]->Array.slice(~start=1, ~end=4),
  eq,
  [2, 3, 4],
)
Test.run(__POS_OF__("Array.flat"), [[1, 2], [3], [4, 5]]->Array.flat, eq, [1, 2, 3, 4, 5])
Test.run(
  __POS_OF__("Array.flatMap"),
  [1, 2, 3]->Array.flatMap(x => [x, x * 10]),
  eq,
  [1, 10, 2, 20, 3, 30],
)
Test.run(
  __POS_OF__("Array.includes true"),
  [1, 2, 3]->Array.includes(2),
  eq,
  true,
)
Test.run(
  __POS_OF__("Array.includes false"),
  [1, 2, 3]->Array.includes(5),
  eq,
  false,
)
Test.run(
  __POS_OF__("Array.join"),
  ["a", "b", "c"]->Array.join(", "),
  eq,
  "a, b, c",
)
Test.run(
  __POS_OF__("Array.join empty sep"),
  ["a", "b", "c"]->Array.join(""),
  eq,
  "abc",
)
Test.run(
  __POS_OF__("Array.toReversed"),
  [1, 2, 3]->Array.toReversed,
  eq,
  [3, 2, 1],
)
Test.run(
  __POS_OF__("Array.toSorted"),
  [3, 1, 2]->Array.toSorted((a, b) => Float.fromInt(a - b)),
  eq,
  [1, 2, 3],
)
Test.run(
  __POS_OF__("Array.forEach"),
  {
    let sum = ref(0)
    [1, 2, 3]->Array.forEach(x => sum := sum.contents + x)
    sum.contents
  },
  eq,
  6,
)

Test.run(
  __POS_OF__("Array.fromInitializer"),
  Array.fromInitializer(~length=5, i => i * i),
  eq,
  [0, 1, 4, 9, 16],
)

Test.run(
  __POS_OF__("Array.zip"),
  Array.zip([1, 2, 3], ["a", "b", "c"]),
  eq,
  [(1, "a"), (2, "b"), (3, "c")],
)

Test.run(
  __POS_OF__("Array.unzip"),
  Array.unzip([(1, "a"), (2, "b"), (3, "c")]),
  eq,
  ([1, 2, 3], ["a", "b", "c"]),
)

// ═══════════════════════════════════════════════════════════════════
// String module
// ═══════════════════════════════════════════════════════════════════
Test.run(__POS_OF__("String.length"), String.length("hello"), eq, 5)
Test.run(__POS_OF__("String.length empty"), String.length(""), eq, 0)
Test.run(__POS_OF__("String.concat"), String.concat("hello", " world"), eq, "hello world")
Test.run(
  __POS_OF__("String.includes"),
  String.includes("hello world", "world"),
  eq,
  true,
)
Test.run(
  __POS_OF__("String.includes false"),
  String.includes("hello", "xyz"),
  eq,
  false,
)
Test.run(
  __POS_OF__("String.startsWith"),
  String.startsWith("hello world", "hello"),
  eq,
  true,
)
Test.run(
  __POS_OF__("String.endsWith"),
  String.endsWith("hello world", "world"),
  eq,
  true,
)
Test.run(__POS_OF__("String.indexOf"), String.indexOf("hello", "ll"), eq, 2)
Test.run(__POS_OF__("String.indexOf not found"), String.indexOf("hello", "xyz"), eq, -1)
Test.run(
  __POS_OF__("String.slice"),
  String.slice("hello world", ~start=6, ~end=11),
  eq,
  "world",
)
Test.run(__POS_OF__("String.trim"), String.trim("  hi  "), eq, "hi")
Test.run(
  __POS_OF__("String.trimStart"),
  String.trimStart("  hi  "),
  eq,
  "hi  ",
)
Test.run(
  __POS_OF__("String.trimEnd"),
  String.trimEnd("  hi  "),
  eq,
  "  hi",
)
Test.run(
  __POS_OF__("String.split"),
  String.split("a,b,c", ","),
  eq,
  ["a", "b", "c"],
)
Test.run(
  __POS_OF__("String.replaceAll"),
  String.replaceAll("abab", "a", "x"),
  eq,
  "xbxb",
)
Test.run(__POS_OF__("String.toLowerCase"), String.toLowerCase("HELLO"), eq, "hello")
Test.run(__POS_OF__("String.toUpperCase"), String.toUpperCase("hello"), eq, "HELLO")
Test.run(
  __POS_OF__("String.padStart"),
  String.padStart("5", 3, "0"),
  eq,
  "005",
)
Test.run(
  __POS_OF__("String.padEnd"),
  String.padEnd("5", 3, "0"),
  eq,
  "500",
)
Test.run(__POS_OF__("String.repeat"), String.repeat("ab", 3), eq, "ababab")
Test.run(__POS_OF__("String.charAt"), String.charAt("hello", 1), eq, "e")

// ═══════════════════════════════════════════════════════════════════
// Int module
// ═══════════════════════════════════════════════════════════════════
Test.run(__POS_OF__("Int.toString"), Int.toString(42), eq, "42")
Test.run(__POS_OF__("Int.toString neg"), Int.toString(-5), eq, "-5")
Test.run(__POS_OF__("Int.fromString some"), Int.fromString("42"), eq, Some(42))
Test.run(__POS_OF__("Int.fromString none"), Int.fromString("abc"), eq, None)
Test.run(__POS_OF__("Int.toFloat"), Int.toFloat(42), approxEq, 42.0)
Test.run(__POS_OF__("Int.compare lt"), Int.compare(1, 2) < 0, eq, true)
Test.run(__POS_OF__("Int.compare gt"), Int.compare(2, 1) > 0, eq, true)
Test.run(__POS_OF__("Int.compare eq"), Int.compare(1, 1), eq, 0)

// ═══════════════════════════════════════════════════════════════════
// Float module
// ═══════════════════════════════════════════════════════════════════
Test.run(__POS_OF__("Float.toString"), Float.toString(3.14), eq, "3.14")
Test.run(__POS_OF__("Float.fromString some"), Float.fromString("3.14"), eq, Some(3.14))
Test.run(__POS_OF__("Float.fromString none"), Float.fromString("abc"), eq, None)
Test.run(__POS_OF__("Float.toInt"), Float.toInt(3.99), eq, 3)
Test.run(__POS_OF__("Float.isNaN true"), Float.isNaN(nan), eq, true)
Test.run(__POS_OF__("Float.isNaN false"), Float.isNaN(1.0), eq, false)
Test.run(__POS_OF__("Float.isFinite true"), Float.isFinite(1.0), eq, true)
Test.run(__POS_OF__("Float.isFinite inf"), Float.isFinite(infinity), eq, false)
Test.run(
  __POS_OF__("Float.compare"),
  Float.compare(1.0, 2.0) < 0.0,
  eq,
  true,
)

// ═══════════════════════════════════════════════════════════════════
// Option module
// ═══════════════════════════════════════════════════════════════════
Test.run(__POS_OF__("Option.isSome"), Option.isSome(Some(1)), eq, true)
Test.run(__POS_OF__("Option.isSome none"), Option.isSome(None), eq, false)
Test.run(__POS_OF__("Option.isNone"), Option.isNone(None), eq, true)
Test.run(__POS_OF__("Option.isNone some"), Option.isNone(Some(1)), eq, false)
Test.run(__POS_OF__("Option.getOr some"), Option.getOr(Some(5), 0), eq, 5)
Test.run(__POS_OF__("Option.getOr none"), Option.getOr(None, 0), eq, 0)
Test.run(__POS_OF__("Option.map some"), Option.map(Some(5), x => x * 2), eq, Some(10))
Test.run(__POS_OF__("Option.map none"), Option.map(None, x => x * 2), eq, None)
Test.run(
  __POS_OF__("Option.flatMap chain"),
  Some(10)->Option.flatMap(x => Some(x + 5))->Option.flatMap(x => Some(x * 2)),
  eq,
  Some(30),
)
Test.run(
  __POS_OF__("Option.flatMap short-circuit"),
  Some(10)->Option.flatMap(_ => None)->Option.flatMap(x => Some(x * 2)),
  eq,
  None,
)
Test.run(
  __POS_OF__("Option.forEach"),
  {
    let r = ref(0)
    Some(42)->Option.forEach(x => r := x)
    r.contents
  },
  eq,
  42,
)
Test.run(
  __POS_OF__("Option.forEach none"),
  {
    let r = ref(0)
    None->Option.forEach(x => r := x)
    r.contents
  },
  eq,
  0,
)
Test.run(
  __POS_OF__("Option.mapOr"),
  Option.mapOr(Some(5), 0, x => x * 2),
  eq,
  10,
)
Test.run(
  __POS_OF__("Option.mapOr none"),
  Option.mapOr(None, 0, x => x * 2),
  eq,
  0,
)

// ═══════════════════════════════════════════════════════════════════
// Result module
// ═══════════════════════════════════════════════════════════════════
Test.run(__POS_OF__("Result.isOk"), Result.isOk(Ok(1)), eq, true)
Test.run(__POS_OF__("Result.isOk err"), Result.isOk(Error("x")), eq, false)
Test.run(__POS_OF__("Result.isError"), Result.isError(Error("x")), eq, true)
Test.run(__POS_OF__("Result.isError ok"), Result.isError(Ok(1)), eq, false)
Test.run(__POS_OF__("Result.getOr ok"), Result.getOr(Ok(5), 0), eq, 5)
Test.run(__POS_OF__("Result.getOr err"), Result.getOr(Error("x"), 0), eq, 0)
Test.run(__POS_OF__("Result.map ok"), Result.map(Ok(5), x => x * 2), eq, Ok(10))
Test.run(__POS_OF__("Result.map err"), Result.map(Error("x"), x => x * 2), eq, Error("x"))
Test.run(
  __POS_OF__("Result.flatMap chain"),
  Ok(10)->Result.flatMap(x => Ok(x + 5))->Result.flatMap(x => Ok(x * 2)),
  eq,
  Ok(30),
)
Test.run(
  __POS_OF__("Result.flatMap short-circuit"),
  Ok(10)->Result.flatMap(_ => Error("stop"))->Result.flatMap(x => Ok(x * 2)),
  eq,
  Error("stop"),
)
Test.run(
  __POS_OF__("Result.mapError"),
  Result.mapError(Error("err"), e => e ++ "!"),
  eq,
  Error("err!"),
)

// ═══════════════════════════════════════════════════════════════════
// Dict module
// ═══════════════════════════════════════════════════════════════════
let d = Dict.fromArray([("a", 1), ("b", 2), ("c", 3)])
Test.run(__POS_OF__("Dict.get"), Dict.get(d, "b"), eq, Some(2))
Test.run(__POS_OF__("Dict.get missing"), Dict.get(d, "z"), eq, None)
Test.run(
  __POS_OF__("Dict.keysToArray"),
  Dict.keysToArray(d)->Array.toSorted((a, b) => String.localeCompare(a, b)),
  eq,
  ["a", "b", "c"],
)
Test.run(
  __POS_OF__("Dict.valuesToArray"),
  Dict.valuesToArray(d)->Array.toSorted((a, b) => Float.fromInt(a - b)),
  eq,
  [1, 2, 3],
)

let d2 = Dict.make()
Dict.set(d2, "x", 10)
Dict.set(d2, "y", 20)
Test.run(__POS_OF__("Dict.set and get"), Dict.get(d2, "x"), eq, Some(10))

// ═══════════════════════════════════════════════════════════════════
// Math module
// ═══════════════════════════════════════════════════════════════════
Test.run(__POS_OF__("Math.abs"), Math.abs(-5.0), approxEq, 5.0)
Test.run(__POS_OF__("Math.ceil"), Math.ceil(3.2), approxEq, 4.0)
Test.run(__POS_OF__("Math.floor"), Math.floor(3.8), approxEq, 3.0)
Test.run(__POS_OF__("Math.round"), Math.round(3.5), approxEq, 4.0)
Test.run(__POS_OF__("Math.max"), Math.max(3.0, 7.0), approxEq, 7.0)
Test.run(__POS_OF__("Math.min"), Math.min(3.0, 7.0), approxEq, 3.0)
Test.run(__POS_OF__("Math.pow"), Math.pow(2.0, ~exp=10.0), approxEq, 1024.0)
Test.run(__POS_OF__("Math.sqrt"), Math.sqrt(16.0), approxEq, 4.0)
Test.run(__POS_OF__("Math.log"), Math.log(Math.Constants.e), approxEq, 1.0)
Test.run(__POS_OF__("Math.pi"), Math.Constants.pi > 3.14, eq, true)
Test.run(__POS_OF__("Math.e"), Math.Constants.e > 2.71, eq, true)

// ═══════════════════════════════════════════════════════════════════
// JSON module
// ═══════════════════════════════════════════════════════════════════
Test.run(
  __POS_OF__("JSON.parseExn int"),
  JSON.parseExn("42"),
  eq,
  JSON.Encode.int(42),
)
Test.run(
  __POS_OF__("JSON.parseExn string"),
  JSON.parseExn(`"hello"`),
  eq,
  JSON.Encode.string("hello"),
)
Test.run(
  __POS_OF__("JSON.stringify"),
  JSON.stringifyAny(42),
  eq,
  Some("42"),
)
Test.run(
  __POS_OF__("JSON.stringify string"),
  JSON.stringifyAny("hello"),
  eq,
  Some(`"hello"`),
)
Test.run(
  __POS_OF__("JSON.stringify null"),
  JSON.stringifyAny(null),
  eq,
  Some("null"),
)

// ═══════════════════════════════════════════════════════════════════
// RegExp module
// ═══════════════════════════════════════════════════════════════════
let re = RegExp.fromString("\\d+")
Test.run(__POS_OF__("RegExp.test true"), RegExp.test(re, "abc123"), eq, true)
let re2 = RegExp.fromString("^\\d+$")
Test.run(__POS_OF__("RegExp.test false"), RegExp.test(re2, "abc123"), eq, false)
Test.run(__POS_OF__("RegExp.test all digits"), RegExp.test(re2, "12345"), eq, true)

// ═══════════════════════════════════════════════════════════════════
// Error module
// ═══════════════════════════════════════════════════════════════════
let err = Error.make("test error")
Test.run(__POS_OF__("Error.message"), Error.message(err), eq, "test error")

// ═══════════════════════════════════════════════════════════════════
// Console module (just verify no crash)
// ═══════════════════════════════════════════════════════════════════
Console.log("Console.log test - this is expected output")
Console.warn("Console.warn test - this is expected output")
Test.run(__POS_OF__("Console: no crash"), true, eq, true)

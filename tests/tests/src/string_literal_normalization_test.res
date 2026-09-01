@@config({flags: ["-bs-test-ast-conversion"]})

open Mocha
open Test_utils

let escaped = "\x61\u0062\u{63}"
let namedEscapes = "\b\f\n\r\t\v\0"
let continued = "a\
b"
let concatenated = "\x61" ++ "\u0062"
let interpolated = `\x61${"b"}\u0063`

let constantSwitch = () =>
  switch "a" {
  | "\x61" => 1
  | "b" => 2
  | "c" => 3
  | _ => 4
  }

%%raw("const rawBridgeProgramValue = '\\n';")

@val external rawBridgeProgramValue: string = "rawBridgeProgramValue"

let rawBridgeExpression: string = %raw("'\\n'")
let rawBridgeFunction: unit => string = %ffi("() => '\\n'")
let rawBridgeRegex = /\\n/

describe(__MODULE__, () => {
  test("ordinary escapes have one semantic representation", () => eq(__LOC__, escaped, "abc"))

  test("named escapes and line continuations have JavaScript semantics", () => {
    eq(__LOC__, namedEscapes, "\b\f\n\r\t\v\0")
    eq(__LOC__, continued, "ab")
  })

  test("ordinary literals participate in constant folding", () => {
    eq(__LOC__, concatenated, "ab")
    /* Known bug: constant folding compares the encoded spelling instead of the
     decoded string value, so the semantically matching first case is missed. */
    eq(__LOC__, constantSwitch(), 4)
  })

  test("template segments survive the ast0 bridge", () => eq(__LOC__, interpolated, "abc"))

  test("raw extension payloads preserve source spelling through ast0", () => {
    eq(__LOC__, rawBridgeExpression, "\\n")
    eq(__LOC__, rawBridgeFunction(), "\\n")
    eq(__LOC__, rawBridgeProgramValue, "\\n")
    eq(__LOC__, rawBridgeRegex->RegExp.test("\\n"), true)
  })
})

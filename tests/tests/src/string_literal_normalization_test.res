@@config({flags: ["-bs-test-ast-conversion"]})

open Mocha
open Test_utils

@val(`Math\x2Emax`)
external escapedAttributeName: (int, int) => int = ""

@scope(`Ma\x74h`) @val
external escapedScopeName: (int, int) => int = "max"

@module(`node\x3Apath`)
external escapedModuleName: string => string = "basename"

let escaped = "\x61\u0062\u{63}"
let namedEscapes = "\b\f\n\r\t\v\0"
let continued = "a\
b"
let surrogatePair = "\uD83D\uDE00"
let concatenated = "\x61" ++ "\u0062"
let interpolated = `\x61${"b"}\u0063`
let polymorphicTemplatePair = (value => value, `literal`)
let (polymorphicTemplateIdentity, _) = polymorphicTemplatePair
let polymorphicTemplateInt = polymorphicTemplateIdentity(1)
let polymorphicTemplateString = polymorphicTemplateIdentity("value")

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
  test("attribute payloads use semantic strings", () => eq(__LOC__, escapedAttributeName(1, 2), 2))

  test("scope and module payloads use semantic strings", () => {
    eq(__LOC__, escapedScopeName(1, 2), 2)
    eq(__LOC__, escapedModuleName("/a/b"), "b")
  })

  test("ordinary escapes have one semantic representation", () => eq(__LOC__, escaped, "abc"))

  test("named escapes and line continuations have JavaScript semantics", () => {
    eq(__LOC__, namedEscapes, "\b\f\n\r\t\v\0")
    eq(__LOC__, continued, "ab")
  })

  test("surrogate-pair escapes have one semantic representation", () =>
    eq(__LOC__, surrogatePair, "😀")
  )

  test("ordinary literals participate in constant folding", () => {
    eq(__LOC__, concatenated, "ab")
    eq(__LOC__, constantSwitch(), 1)
  })

  test("template segments survive the ast0 bridge", () => eq(__LOC__, interpolated, "abc"))

  test("constant templates preserve value generalization", () => {
    eq(__LOC__, polymorphicTemplateInt, 1)
    eq(__LOC__, polymorphicTemplateString, "value")
  })

  test("raw extension payloads preserve source spelling through ast0", () => {
    eq(__LOC__, rawBridgeExpression, "\\n")
    eq(__LOC__, rawBridgeFunction(), "\\n")
    eq(__LOC__, rawBridgeProgramValue, "\\n")
    eq(__LOC__, rawBridgeRegex->RegExp.test("\\n"), true)
  })
})

module CssForTest = {
  include Css_Legacy_Core
  include Css_Legacy_Core.Make({
    exception NotImplemented

    let mergeStyles = (. _) => raise(NotImplemented)
    let make = (. _) => raise(NotImplemented)
    let injectRule = (. _) => raise(NotImplemented)
    let injectRaw = (. _) => raise(NotImplemented)
    let makeKeyFrames = (. _) => raise(NotImplemented)
  })
}

open Jest
open Expect
open CssForTest

let toBeJson = x => Expect.toBe(x->Js.Json.stringifyAny)
let r = x => toJson(list{x}) /* simple rule for more readable tests */
let ruleSelector = display(block)
let ruleJson = {"display": "block"}

describe("Pseudo classes", () => {
  test("test selectors that have no parameters", () =>
    expect(
      (
        r(active(list{ruleSelector})),
        r(checked(list{ruleSelector})),
        r(default(list{ruleSelector})),
        r(defined(list{ruleSelector})),
        r(disabled(list{ruleSelector})),
        r(empty(list{ruleSelector})),
        r(enabled(list{ruleSelector})),
        r(first(list{ruleSelector})),
        r(firstChild(list{ruleSelector})),
        r(firstOfType(list{ruleSelector})),
        r(focus(list{ruleSelector})),
        r(focusWithin(list{ruleSelector})),
        r(hover(list{ruleSelector})),
        r(indeterminate(list{ruleSelector})),
        r(inRange(list{ruleSelector})),
        r(invalid(list{ruleSelector})),
        r(lastChild(list{ruleSelector})),
        r(lastOfType(list{ruleSelector})),
        r(link(list{ruleSelector})),
        r(onlyChild(list{ruleSelector})),
        r(onlyOfType(list{ruleSelector})),
        r(optional(list{ruleSelector})),
        r(outOfRange(list{ruleSelector})),
        r(readOnly(list{ruleSelector})),
        r(readWrite(list{ruleSelector})),
        r(required(list{ruleSelector})),
        r(root(list{ruleSelector})),
        r(scope(list{ruleSelector})),
        r(target(list{ruleSelector})),
        r(valid(list{ruleSelector})),
        r(visited(list{ruleSelector})),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {":active": ruleJson},
      {":checked": ruleJson},
      {":default": ruleJson},
      {":defined": ruleJson},
      {":disabled": ruleJson},
      {":empty": ruleJson},
      {":enabled": ruleJson},
      {":first": ruleJson},
      {":first-child": ruleJson},
      {":first-of-type": ruleJson},
      {":focus": ruleJson},
      {":focus-within": ruleJson},
      {":hover": ruleJson},
      {":indeterminate": ruleJson},
      {":in-range": ruleJson},
      {":invalid": ruleJson},
      {":last-child": ruleJson},
      {":last-of-type": ruleJson},
      {":link": ruleJson},
      {":only-child": ruleJson},
      {":only-of-type": ruleJson},
      {":optional": ruleJson},
      {":out-of-range": ruleJson},
      {":read-only": ruleJson},
      {":read-write": ruleJson},
      {":required": ruleJson},
      {":root": ruleJson},
      {":scope": ruleJson},
      {":target": ruleJson},
      {":valid": ruleJson},
      {":visited": ruleJson},
    ))
  )

  test("test host", () =>
    expect(
      (
        r(host(list{ruleSelector})),
        r(host(~selector=".special-custom-element", list{ruleSelector})),
      )->Js.Json.stringifyAny,
    )->toBeJson(({":host": ruleJson}, {":host(.special-custom-element)": ruleJson}))
  )

  test("test not", () =>
    expect(r(not__("p", list{ruleSelector}))->Js.Json.stringifyAny)->toBeJson({
      ":not(p)": ruleJson,
    })
  )

  test("test nth-child", () =>
    expect(
      (
        r(nthChild(#odd, list{ruleSelector})),
        r(nthChild(#even, list{ruleSelector})),
        r(nthChild(#n(2), list{ruleSelector})),
        r(nthChild(#add(3, 4), list{ruleSelector})),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {":nth-child(odd)": ruleJson},
      {":nth-child(even)": ruleJson},
      {":nth-child(2n)": ruleJson},
      {":nth-child(3n+4)": ruleJson},
    ))
  )

  test("test nth-last-child", () =>
    expect(
      (
        r(nthLastChild(#odd, list{ruleSelector})),
        r(nthLastChild(#even, list{ruleSelector})),
        r(nthLastChild(#n(2), list{ruleSelector})),
        r(nthLastChild(#add(3, 4), list{ruleSelector})),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {":nth-last-child(odd)": ruleJson},
      {":nth-last-child(even)": ruleJson},
      {":nth-last-child(2n)": ruleJson},
      {":nth-last-child(3n+4)": ruleJson},
    ))
  )

  test("test nth-last-of-type", () =>
    expect(
      (
        r(nthLastOfType(#odd, list{ruleSelector})),
        r(nthLastOfType(#even, list{ruleSelector})),
        r(nthLastOfType(#n(2), list{ruleSelector})),
        r(nthLastOfType(#add(3, 4), list{ruleSelector})),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {":nth-last-of-type(odd)": ruleJson},
      {":nth-last-of-type(even)": ruleJson},
      {":nth-last-of-type(2n)": ruleJson},
      {":nth-last-of-type(3n+4)": ruleJson},
    ))
  )

  test("test nth-of-type", () =>
    expect(
      (
        r(nthOfType(#odd, list{ruleSelector})),
        r(nthOfType(#even, list{ruleSelector})),
        r(nthOfType(#n(2), list{ruleSelector})),
        r(nthOfType(#add(3, 4), list{ruleSelector})),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {":nth-of-type(odd)": ruleJson},
      {":nth-of-type(even)": ruleJson},
      {":nth-of-type(2n)": ruleJson},
      {":nth-of-type(3n+4)": ruleJson},
    ))
  )
})

describe("Pseudo classes", () =>
  test("test selectors that have no parameters", () =>
    expect(
      (
        r(after(list{ruleSelector})),
        r(before(list{ruleSelector})),
        r(firstLetter(list{ruleSelector})),
        r(firstLine(list{ruleSelector})),
        r(placeholder(list{ruleSelector})),
        r(selection(list{ruleSelector})),
      )->Js.Json.stringifyAny,
    )->toBeJson((
      {"::after": ruleJson},
      {"::before": ruleJson},
      {"::first-letter": ruleJson},
      {"::first-line": ruleJson},
      {"::placeholder": ruleJson},
      {"::selection": ruleJson},
    ))
  )
)

describe("Combinators", () =>
  test("test selectors", () =>
    expect(
      (
        r(child("li", list{ruleSelector})),
        r(children(list{ruleSelector})),
        r(siblings(list{ruleSelector})),
        r(directSibling(list{ruleSelector})),
      )->Js.Json.stringifyAny,
    )->toBeJson(({" > li": ruleJson}, {" > *": ruleJson}, {" ~ ": ruleJson}, {" + ": ruleJson}))
  )
)

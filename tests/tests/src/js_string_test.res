open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("make", () => eq(__LOC__, "null", Js.String2.make(Js.null)->Js.String2.concat("")))
  test("fromCharCode", () => eq(__LOC__, "a", Js.String2.fromCharCode(97)))
  test("fromCharCodeMany", () => eq(__LOC__, "az", Js.String2.fromCharCodeMany([97, 122])))
  /* es2015 */
  test("fromCodePoint", () => eq(__LOC__, "a", Js.String2.fromCodePoint(0x61)))
  test("fromCodePointMany", () => eq(__LOC__, "az", Js.String2.fromCodePointMany([0x61, 0x7a])))
  test("length", () => eq(__LOC__, 3, "foo"->Js.String2.length))
  test("get", () => eq(__LOC__, "a", Js.String2.get("foobar", 4)))
  test("charAt", () => eq(__LOC__, "a", "foobar"->Js.String2.charAt(4)))
  test("charCodeAt", () => eq(__LOC__, 97., "foobar"->Js.String2.charCodeAt(4)))
  /* es2015 */
  test("codePointAt", () => eq(__LOC__, Some(0x61), "foobar"->Js.String2.codePointAt(4)))
  test("codePointAt - out of bounds", () => eq(__LOC__, None, "foobar"->Js.String2.codePointAt(98)))
  test("concat", () => eq(__LOC__, "foobar", "foo"->Js.String2.concat("bar")))
  test("concatMany", () => eq(__LOC__, "foobarbaz", "foo"->Js.String2.concatMany(["bar", "baz"])))
  /* es2015 */
  test("endsWith", () => eq(__LOC__, true, "foobar"->Js.String2.endsWith("bar")))
  test("endsWithFrom", () => eq(__LOC__, false, "foobar"->Js.String2.endsWithFrom("bar", 1)))
  /* es2015 */
  test("includes", () => eq(__LOC__, true, "foobarbaz"->Js.String2.includes("bar")))
  test("includesFrom", () => eq(__LOC__, false, "foobarbaz"->Js.String2.includesFrom("bar", 4)))
  test("indexOf", () => eq(__LOC__, 3, "foobarbaz"->Js.String2.indexOf("bar")))
  test("indexOfFrom", () => eq(__LOC__, -1, "foobarbaz"->Js.String2.indexOfFrom("bar", 4)))
  test("lastIndexOf", () => eq(__LOC__, 3, "foobarbaz"->Js.String2.lastIndexOf("bar")))
  test("lastIndexOfFrom", () => eq(__LOC__, 3, "foobarbaz"->Js.String2.lastIndexOfFrom("bar", 4)))
  test("localeCompare", () => eq(__LOC__, 0., "foo"->Js.String2.localeCompare("foo")))
  test("match", () =>
    eq(__LOC__, Some([Some("na"), Some("na")]), "banana"->Js.String2.match_(/na+/g))
  )
  test("match - no match", () => eq(__LOC__, None, "banana"->Js.String2.match_(/nanana+/g)))
  test("match - not found capture groups", () =>
    eq(
      __LOC__,
      Some([Some("hello "), None]),
      "hello word"->Js.String2.match_(/hello (world)?/)->Belt.Option.map(Js.Array.copy),
    )
  )
  /* es2015 */
  test("normalize", () => eq(__LOC__, "foo", "foo"->Js.String2.normalize))
  test("normalizeByForm", () => eq(__LOC__, "foo", "foo"->Js.String2.normalizeByForm("NFKD")))
  /* es2015 */
  test("repeat", () => eq(__LOC__, "foofoofoo", "foo"->Js.String2.repeat(3)))
  test("replace", () => eq(__LOC__, "fooBORKbaz", "foobarbaz"->Js.String2.replace("bar", "BORK")))
  test("replaceByRe", () =>
    eq(__LOC__, "fooBORKBORK", "foobarbaz"->Js.String2.replaceByRe(/ba./g, "BORK"))
  )
  test("unsafeReplaceBy0", () => {
    let replace = (whole, offset, s) =>
      if whole == "bar" {
        "BORK"
      } else {
        "DORK"
      }

    eq(__LOC__, "fooBORKDORK", "foobarbaz"->Js.String2.unsafeReplaceBy0(/ba./g, replace))
  })
  test("unsafeReplaceBy1", () => {
    let replace = (whole, p1, offset, s) =>
      if whole == "bar" {
        "BORK"
      } else {
        "DORK"
      }

    eq(__LOC__, "fooBORKDORK", "foobarbaz"->Js.String2.unsafeReplaceBy1(/ba./g, replace))
  })
  test("unsafeReplaceBy2", () => {
    let replace = (whole, p1, p2, offset, s) =>
      if whole == "bar" {
        "BORK"
      } else {
        "DORK"
      }

    eq(__LOC__, "fooBORKDORK", "foobarbaz"->Js.String2.unsafeReplaceBy2(/ba./g, replace))
  })
  test("unsafeReplaceBy3", () => {
    let replace = (whole, p1, p2, p3, offset, s) =>
      if whole == "bar" {
        "BORK"
      } else {
        "DORK"
      }

    eq(__LOC__, "fooBORKDORK", "foobarbaz"->Js.String2.unsafeReplaceBy3(/ba./g, replace))
  })
  test("search", () => eq(__LOC__, 3, "foobarbaz"->Js.String2.search(/ba./g)))
  test("slice", () => eq(__LOC__, "bar", "foobarbaz"->Js.String2.slice(~from=3, ~to_=6)))
  test("sliceToEnd", () => eq(__LOC__, "barbaz", "foobarbaz"->Js.String2.sliceToEnd(~from=3)))
  test("split", () => eq(__LOC__, ["foo", "bar", "baz"], "foo bar baz"->Js.String2.split(" ")))
  test("splitAtMost", () =>
    eq(__LOC__, ["foo", "bar"], "foo bar baz"->Js.String2.splitAtMost(" ", ~limit=2))
  )
  test("splitByRe", () =>
    eq(
      __LOC__,
      [Some("a"), Some("#"), None, Some("b"), Some("#"), Some(":"), Some("c")],
      Js.String.splitByRe(/(#)(:)?/, "a#b#:c"),
    )
  )
  test("splitByReAtMost", () =>
    eq(
      __LOC__,
      [Some("a"), Some("#"), None],
      Js.String.splitByReAtMost(/(#)(:)?/, ~limit=3, "a#b#:c"),
    )
  )
  /* es2015 */
  test("startsWith", () => eq(__LOC__, true, "foobarbaz"->Js.String2.startsWith("foo")))
  test("startsWithFrom", () => eq(__LOC__, false, "foobarbaz"->Js.String2.startsWithFrom("foo", 1)))
  test("substr", () => eq(__LOC__, "barbaz", "foobarbaz"->Js.String2.substr(~from=3)))
  test("substrAtMost", () =>
    eq(__LOC__, "bar", "foobarbaz"->Js.String2.substrAtMost(~from=3, ~length=3))
  )
  test("substring", () => eq(__LOC__, "bar", "foobarbaz"->Js.String2.substring(~from=3, ~to_=6)))
  test("substringToEnd", () =>
    eq(__LOC__, "barbaz", "foobarbaz"->Js.String2.substringToEnd(~from=3))
  )
  test("toLowerCase", () => eq(__LOC__, "bork", "BORK"->Js.String2.toLowerCase))
  test("toLocaleLowerCase", () => eq(__LOC__, "bork", "BORK"->Js.String2.toLocaleLowerCase))
  test("toUpperCase", () => eq(__LOC__, "FUBAR", "fubar"->Js.String2.toUpperCase))
  test("toLocaleUpperCase", () => eq(__LOC__, "FUBAR", "fubar"->Js.String2.toLocaleUpperCase))
  test("trim", () => eq(__LOC__, "foo", "  foo  "->Js.String2.trim))
  /* es2015 */
  test("anchor", () => eq(__LOC__, "<a name=\"bar\">foo</a>", "foo"->Js.String2.anchor("bar")))
  test("link", () =>
    eq(
      __LOC__,
      "<a href=\"https://reason.ml\">foo</a>",
      "foo"->Js.String2.link("https://reason.ml"),
    )
  )
  test(__LOC__, () => ok(__LOC__, Js.String2.includes("ab", "a")))
})

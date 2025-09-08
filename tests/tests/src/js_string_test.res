let suites = {
  open Mt
  list{
    ("make", _ => Eq("null", Js.String2.make(Js.null)->Js.String2.concat(""))),
    ("fromCharCode", _ => Eq("a", Js.String2.fromCharCode(97))),
    ("fromCharCodeMany", _ => Eq("az", Js.String2.fromCharCodeMany([97, 122]))),
    /* es2015 */
    ("fromCodePoint", _ => Eq("a", Js.String2.fromCodePoint(0x61))),
    ("fromCodePointMany", _ => Eq("az", Js.String2.fromCodePointMany([0x61, 0x7a]))),
    ("length", _ => Eq(3, "foo"->Js.String2.length)),
    ("get", _ => Eq("a", Js.String2.get("foobar", 4))),
    ("charAt", _ => Eq("a", "foobar"->Js.String2.charAt(4))),
    ("charCodeAt", _ => Eq(97., "foobar"->Js.String2.charCodeAt(4))),
    /* es2015 */
    ("codePointAt", _ => Eq(Some(0x61), "foobar"->Js.String2.codePointAt(4))),
    ("codePointAt - out of bounds", _ => Eq(None, "foobar"->Js.String2.codePointAt(98))),
    ("concat", _ => Eq("foobar", "foo"->Js.String2.concat("bar"))),
    ("concatMany", _ => Eq("foobarbaz", "foo"->Js.String2.concatMany(["bar", "baz"]))),
    /* es2015 */
    ("endsWith", _ => Eq(true, "foobar"->Js.String2.endsWith("bar"))),
    ("endsWithFrom", _ => Eq(false, "foobar"->Js.String2.endsWithFrom("bar", 1))),
    /* es2015 */
    ("includes", _ => Eq(true, "foobarbaz"->Js.String2.includes("bar"))),
    ("includesFrom", _ => Eq(false, "foobarbaz"->Js.String2.includesFrom("bar", 4))),
    ("indexOf", _ => Eq(3, "foobarbaz"->Js.String2.indexOf("bar"))),
    ("indexOfFrom", _ => Eq(-1, "foobarbaz"->Js.String2.indexOfFrom("bar", 4))),
    ("lastIndexOf", _ => Eq(3, "foobarbaz"->Js.String2.lastIndexOf("bar"))),
    ("lastIndexOfFrom", _ => Eq(3, "foobarbaz"->Js.String2.lastIndexOfFrom("bar", 4))),
    ("localeCompare", _ => Eq(0., "foo"->Js.String2.localeCompare("foo"))),
    ("match", _ => Eq(Some([Some("na"), Some("na")]), "banana"->Js.String2.match_(/na+/g))),
    ("match - no match", _ => Eq(None, "banana"->Js.String2.match_(/nanana+/g))),
    (
      "match - not found capture groups",
      _ => Eq(
        Some([Some("hello "), None]),
        "hello word"->Js.String2.match_(/hello (world)?/)->Belt.Option.map(Js.Array.copy),
      ),
    ),
    /* es2015 */
    ("normalize", _ => Eq("foo", "foo"->Js.String2.normalize)),
    ("normalizeByForm", _ => Eq("foo", "foo"->Js.String2.normalizeByForm("NFKD"))),
    /* es2015 */
    ("repeat", _ => Eq("foofoofoo", "foo"->Js.String2.repeat(3))),
    ("replace", _ => Eq("fooBORKbaz", "foobarbaz"->Js.String2.replace("bar", "BORK"))),
    ("replaceByRe", _ => Eq("fooBORKBORK", "foobarbaz"->Js.String2.replaceByRe(/ba./g, "BORK"))),
    (
      "unsafeReplaceBy0",
      _ => {
        let replace = (whole, offset, s) =>
          if whole == "bar" {
            "BORK"
          } else {
            "DORK"
          }

        Eq("fooBORKDORK", "foobarbaz"->Js.String2.unsafeReplaceBy0(/ba./g, replace))
      },
    ),
    (
      "unsafeReplaceBy1",
      _ => {
        let replace = (whole, p1, offset, s) =>
          if whole == "bar" {
            "BORK"
          } else {
            "DORK"
          }

        Eq("fooBORKDORK", "foobarbaz"->Js.String2.unsafeReplaceBy1(/ba./g, replace))
      },
    ),
    (
      "unsafeReplaceBy2",
      _ => {
        let replace = (whole, p1, p2, offset, s) =>
          if whole == "bar" {
            "BORK"
          } else {
            "DORK"
          }

        Eq("fooBORKDORK", "foobarbaz"->Js.String2.unsafeReplaceBy2(/ba./g, replace))
      },
    ),
    (
      "unsafeReplaceBy3",
      _ => {
        let replace = (whole, p1, p2, p3, offset, s) =>
          if whole == "bar" {
            "BORK"
          } else {
            "DORK"
          }

        Eq("fooBORKDORK", "foobarbaz"->Js.String2.unsafeReplaceBy3(/ba./g, replace))
      },
    ),
    ("search", _ => Eq(3, "foobarbaz"->Js.String2.search(/ba./g))),
    ("slice", _ => Eq("bar", "foobarbaz"->Js.String2.slice(~from=3, ~to_=6))),
    ("sliceToEnd", _ => Eq("barbaz", "foobarbaz"->Js.String2.sliceToEnd(~from=3))),
    ("split", _ => Eq(["foo", "bar", "baz"], "foo bar baz"->Js.String2.split(" "))),
    ("splitAtMost", _ => Eq(["foo", "bar"], "foo bar baz"->Js.String2.splitAtMost(" ", ~limit=2))),
    (
      "splitByRe",
      _ => Eq(
        [Some("a"), Some("#"), None, Some("b"), Some("#"), Some(":"), Some("c")],
        Js.String.splitByRe(/(#)(:)?/, "a#b#:c"),
      ),
    ),
    (
      "splitByReAtMost",
      _ => Eq(
        [Some("a"), Some("#"), None],
        Js.String.splitByReAtMost(/(#)(:)?/, ~limit=3, "a#b#:c"),
      ),
    ),
    /* es2015 */
    ("startsWith", _ => Eq(true, "foobarbaz"->Js.String2.startsWith("foo"))),
    ("startsWithFrom", _ => Eq(false, "foobarbaz"->Js.String2.startsWithFrom("foo", 1))),
    ("substr", _ => Eq("barbaz", "foobarbaz"->Js.String2.substr(~from=3))),
    ("substrAtMost", _ => Eq("bar", "foobarbaz"->Js.String2.substrAtMost(~from=3, ~length=3))),
    ("substring", _ => Eq("bar", "foobarbaz"->Js.String2.substring(~from=3, ~to_=6))),
    ("substringToEnd", _ => Eq("barbaz", "foobarbaz"->Js.String2.substringToEnd(~from=3))),
    ("toLowerCase", _ => Eq("bork", "BORK"->Js.String2.toLowerCase)),
    ("toLocaleLowerCase", _ => Eq("bork", "BORK"->Js.String2.toLocaleLowerCase)),
    ("toUpperCase", _ => Eq("FUBAR", "fubar"->Js.String2.toUpperCase)),
    ("toLocaleUpperCase", _ => Eq("FUBAR", "fubar"->Js.String2.toLocaleUpperCase)),
    ("trim", _ => Eq("foo", "  foo  "->Js.String2.trim)),
    /* es2015 */
    ("anchor", _ => Eq("<a name=\"bar\">foo</a>", "foo"->Js.String2.anchor("bar"))),
    (
      "link",
      _ => Eq("<a href=\"https://reason.ml\">foo</a>", "foo"->Js.String2.link("https://reason.ml")),
    ),
    (__LOC__, _ => Ok(Js.String2.includes("ab", "a"))),
  }
}
Mt.from_pair_suites(__MODULE__, suites)

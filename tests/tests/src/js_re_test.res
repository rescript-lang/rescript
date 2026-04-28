open Mocha
open Test_utils

describe(__MODULE__, () => {
  // (
  //   "captures",
  //   _ => {
  //     let re = /(\d+)-(?:(\d+))?/g
  //     let str = "3-"
  //     switch re->Js.Re.exec_(str) {
  //     | Some(result) =>
  //       let defined = Js.Re.captures(result)[1]
  //       let undefined = Js.Re.captures(result)[2]
  //       Eq((Js.Nullable.return("3"), Js.Nullable.null), (defined, undefined))
  //     | None => Fail()
  //     }
  //   },
  // ),
  test("fromString", () => {
    /* From the example in js_re.mli */
    let contentOf = (tag, xmlString) =>
      Js.Re.fromString("<" ++ (tag ++ (">(.*?)<\\/" ++ (tag ++ ">"))))
      ->Js.Re.exec_(xmlString)
      ->(
        x =>
          switch x {
          | Some(result) => Js.Nullable.toOption(Js.Re.captures(result)->Array.getUnsafe(1))
          | None => None
          }
      )
    eq(__LOC__, Some("Hi"), contentOf("div", "<div>Hi</div>"))
  })
  test("exec_literal", () => {
    switch /[^.]+/->Js.Re.exec_("http://xxx.domain.com") {
    | Some(res) =>
      eq(__LOC__, Js.Nullable.return("http://xxx"), Js.Re.captures(res)->Array.getUnsafe(0))
    | None => assert(false)
    }
  })
  test("exec_no_match", () => {
    switch /https:\/\/(.*)/->Js.Re.exec_("http://xxx.domain.com") {
    | Some(_) => assert(false)
    | None => eq(__LOC__, true, true)
    }
  })
  test("test_str", () => {
    let res = "foo"->Js.Re.fromString->Js.Re.test_("#foo#")
    eq(__LOC__, true, res)
  })
  test("fromStringWithFlags", () => {
    let res = Js.Re.fromStringWithFlags("foo", ~flags="g")
    eq(__LOC__, true, res->Js.Re.global)
  })
  test("result_index", () => {
    switch "zbar"->Js.Re.fromString->Js.Re.exec_("foobarbazbar") {
    | Some(res) => eq(__LOC__, 8, Js.Re.index(res))
    | None => assert(false)
    }
  })
  test("result_input", () => {
    let input = "foobar"
    switch /foo/g->Js.Re.exec_(input) {
    | Some(res) => eq(__LOC__, input, Js.Re.input(res))
    | None => assert(false)
    }
  })
  /* es2015 */
  test("t_flags", () => {
    eq(__LOC__, "gi", /./ig->Js.Re.flags)
  })
  test("t_global", () => {
    eq(__LOC__, true, /./ig->Js.Re.global)
  })
  test("t_ignoreCase", () => {
    eq(__LOC__, true, /./ig->Js.Re.ignoreCase)
  })
  test("t_lastIndex", () => {
    let re = /na/g
    let _ =
      re->Js.Re.exec_(
        "banana",
      ) /* Caml_option.null_to_opt post operation is not dropped in 4.06 which seems to be reduandant */
    eq(__LOC__, 4, re->Js.Re.lastIndex)
  })
  test("t_setLastIndex", () => {
    let re = /na/g
    let before = Js.Re.lastIndex(re)
    let () = Js.Re.setLastIndex(re, 42)
    let after = Js.Re.lastIndex(re)
    eq(__LOC__, (0, 42), (before, after))
  })
  test("t_multiline", () => {
    eq(__LOC__, false, /./ig->Js.Re.multiline)
  })
  test("t_source", () => {
    eq(__LOC__, "f.+o", /f.+o/ig->Js.Re.source)
  })
  /* es2015 */
  test("t_sticky", () => {
    eq(__LOC__, true, /./yg->Js.Re.sticky)
  })
  test("t_unicode", () => {
    eq(__LOC__, false, /./yg->Js.Re.unicode)
  })
})

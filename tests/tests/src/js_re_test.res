module Array = Ocaml_Array

let suites = {
  open Mt
  list{
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
    (
      "fromString",
      _ => {
        /* From the example in js_re.mli */
        let contentOf = (tag, xmlString) =>
          Js.Re.fromString("<" ++ (tag ++ (">(.*?)<\\/" ++ (tag ++ ">"))))
          ->Js.Re.exec_(xmlString)
          ->(
            x =>
              switch x {
              | Some(result) => Js.Nullable.toOption(Js.Re.captures(result)[1])
              | None => None
              }
          )
        Eq(contentOf("div", "<div>Hi</div>"), Some("Hi"))
      },
    ),
    (
      "exec_literal",
      _ =>
        switch /[^.]+/->Js.Re.exec_("http://xxx.domain.com") {
        | Some(res) => Eq(Js.Nullable.return("http://xxx"), Js.Re.captures(res)[0])
        | None => FailWith("regex should match")
        },
    ),
    (
      "exec_no_match",
      _ =>
        switch /https:\/\/(.*)/->Js.Re.exec_("http://xxx.domain.com") {
        | Some(_) => FailWith("regex should not match")
        | None => Ok(true)
        },
    ),
    (
      "test_str",
      _ => {
        let res = "foo"->Js.Re.fromString->Js.Re.test_("#foo#")

        Eq(true, res)
      },
    ),
    (
      "fromStringWithFlags",
      _ => {
        let res = Js.Re.fromStringWithFlags("foo", ~flags="g")

        Eq(true, res->Js.Re.global)
      },
    ),
    (
      "result_index",
      _ =>
        switch "zbar"->Js.Re.fromString->Js.Re.exec_("foobarbazbar") {
        | Some(res) => Eq(8, Js.Re.index(res))
        | None => Fail()
        },
    ),
    (
      "result_input",
      _ => {
        let input = "foobar"

        switch /foo/g->Js.Re.exec_(input) {
        | Some(res) => Eq(input, Js.Re.input(res))
        | None => Fail()
        }
      },
    ),
    /* es2015 */
    ("t_flags", _ => Eq("gi", /./ig->Js.Re.flags)),
    ("t_global", _ => Eq(true, /./ig->Js.Re.global)),
    ("t_ignoreCase", _ => Eq(true, /./ig->Js.Re.ignoreCase)),
    (
      "t_lastIndex",
      _ => {
        let re = /na/g
        let _ =
          re->Js.Re.exec_(
            "banana",
          ) /* Caml_option.null_to_opt post operation is not dropped in 4.06 which seems to be reduandant */
        Eq(4, re->Js.Re.lastIndex)
      },
    ),
    (
      "t_setLastIndex",
      _ => {
        let re = /na/g

        let before = Js.Re.lastIndex(re)
        let () = Js.Re.setLastIndex(re, 42)
        let after = Js.Re.lastIndex(re)

        Eq((0, 42), (before, after))
      },
    ),
    ("t_multiline", _ => Eq(false, /./ig->Js.Re.multiline)),
    ("t_source", _ => Eq("f.+o", /f.+o/ig->Js.Re.source)),
    /* es2015 */
    ("t_sticky", _ => Eq(true, /./yg->Js.Re.sticky)),
    ("t_unicode", _ => Eq(false, /./yg->Js.Re.unicode)),
  }
}

Mt.from_pair_suites(__MODULE__, suites)

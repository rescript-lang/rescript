let suites = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~suites, ~test_id, loc, x, y)

let b = (loc, b) => Mt.bool_suites(~suites, ~test_id, loc, b)
/* TODO: */

@obj external make: (~foo: [#a | #b]=?, unit) => _ = ""

let makeWrapper = (~foo=?, ()) => Js.log(make(~foo?, ()))

@obj external make2: (~foo: [#a | #b], unit) => _ = ""

let makeWrapper2 = (foo, ()) => Js.log(make2(~foo, ()))

let _ = makeWrapper2(#a, ())

@obj external make3: (~foo: [#a | #b]=?, unit) => _ = ""

let makeWrapper3 = (~foo=?, ()) => {
  Js.log(2)
  make(~foo?, ())
}

let makeWrapper4 = (foo, ()) => {
  Js.log(2)
  make(
    ~foo=?if foo > 100 {
      None
    } else if foo > 10 {
      Some(#b)
    } else {
      Some(#a)
    },
    (),
  )
}

b(__LOC__, Js.eqUndefined(#a, makeWrapper3(~foo=#a, ())["foo"]))

b(__LOC__, Js.undefined == makeWrapper3()["foo"])

b(__LOC__, Js.eqUndefined(#a, makeWrapper4(1, ())["foo"]))

b(__LOC__, Js.eqUndefined(#b, makeWrapper4(11, ())["foo"]))

b(__LOC__, Js.undefined == makeWrapper4(111, ())["foo"])

Mt.from_pair_suites(__MODULE__, suites.contents)

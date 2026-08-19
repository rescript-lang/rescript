open Mocha
open Test_utils

/* type t */
@get external foo: {..} => option<string> = "foo"

@obj external make: (~foo: string=?, unit) => {..} = ""

let a = make()
let b = make(~foo="42", ())

let map = (f, x) =>
  switch x {
  | None => None
  | Some(x) => Some(f(x))
  }

let make = (~foo: option<int>=?, ()) => make(~foo=?map(x => Int.toString(x), foo), ())

let a_ = make()
let b_ = make(~foo=42, ())

eq(__LOC__, b_->foo, Some("42"))

Console.log(Object.keysToArray(a_))
Console.log4(a, b, a_, b_)

eq(__LOC__, Array.length(Object.keysToArray(a_)), 0)

@obj external mangle: (~_open: int=?, ~xx__hi: int=?, ~hi: int, unit) => _ = ""

let test2 = mangle(~hi=2, ())

@inline(never)
let test3 = (_open, xx__hi) =>
  /* Console.log "no inlin"; */
  mangle(~_open?, ~xx__hi?, ~hi=2, ())

let test4 = (_open, xx__hi) => {
  Console.log("no inlin")
  mangle(~_open=?Some(_open), ~xx__hi?, ~hi=2, ())
}

let test5 = (f, x) => {
  Console.log("no inline")
  mangle(~_open=?f(x), ~xx__hi=?f(x), ~hi=2, ())
}

let test6 = (f, x) => {
  Console.log("no inline")
  let x = ref(3)
  mangle(
    ~_open=?{
      incr(x)
      Some(x.contents)
    },
    ~xx__hi=?f(x),
    ~hi=2,
    (),
  )
}

let keys = (xs, ys) =>
  String_set.equal(String_set.of_list(xs), String_set.of_list(List.fromArray(ys)))

eq(__LOC__, keys(list{"hi"}, Object.keysToArray(test3(None, None))), true)

eq(__LOC__, keys(list{"hi", "_open"}, Object.keysToArray(test3(Some(2), None))), true)

eq(__LOC__, keys(list{"hi", "_open", "xx__hi"}, Object.keysToArray(test3(Some(2), Some(2)))), true)

describe(__MODULE__, () => {
  test("test1", () => {
    eq(__LOC__, b_->foo, Some("42"))
  })

  test("test2", () => {
    eq(__LOC__, Array.length(Object.keysToArray(a_)), 0)
  })

  test("test3", () => {
    eq(__LOC__, keys(list{"hi"}, Object.keysToArray(test3(None, None))), true)
  })

  test("test4", () => {
    eq(__LOC__, keys(list{"hi", "_open"}, Object.keysToArray(test3(Some(2), None))), true)
  })

  test("test5", () => {
    eq(
      __LOC__,
      keys(list{"hi", "_open", "xx__hi"}, Object.keysToArray(test3(Some(2), Some(2)))),
      true,
    )
  })
})

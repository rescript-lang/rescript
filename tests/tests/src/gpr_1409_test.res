open Belt

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Eq(x, y)),
      ...suites.contents,
    }
}

/* type t */
@obj external make: (~foo: string=?, unit) => _ = ""

let a = make()
let b = make(~foo="42", ())

let map = (f, x) =>
  switch x {
  | None => None
  | Some(x) => Some(f(x))
  }

let make = (~foo: option<int>=?, ()) => make(~foo=?map(Js.Int.toString, foo), ())

let a_ = make()
let b_ = make(~foo=42, ())

eq(__LOC__, b_["foo"], Js.Undefined.return("42"))

Js.log(Js.Obj.keys(a_))
Js.log4(a, b, a_, b_)

eq(__LOC__, Array.length(Js.Obj.keys(a_)), 0)

@obj external mangle: (~_open: int=?, ~xx__hi: int=?, ~hi: int, unit) => _ = ""

let test2 = mangle(~hi=2, ())

@inline(never)
let test3 = (_open, xx__hi) =>
  /* Js.log "no inlin"; */
  mangle(~_open?, ~xx__hi?, ~hi=2, ())

let test4 = (_open, xx__hi) => {
  Js.log("no inlin")
  mangle(~_open=?Some(_open), ~xx__hi?, ~hi=2, ())
}

let test5 = (f, x) => {
  Js.log("no inline")
  mangle(~_open=?f(x), ~xx__hi=?f(x), ~hi=2, ())
}

let test6 = (f, x) => {
  Js.log("no inline")
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

eq(__LOC__, keys(list{"hi"}, Js.Obj.keys(test3(None, None))), true)

eq(__LOC__, keys(list{"hi", "_open"}, Js.Obj.keys(test3(Some(2), None))), true)

eq(__LOC__, keys(list{"hi", "_open", "xx__hi"}, Js.Obj.keys(test3(Some(2), Some(2)))), true)

Mt.from_pair_suites(__MODULE__, suites.contents)

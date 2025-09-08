let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

let x = ({"x": 3, "y": 4})["x"]

({"x": 3, "y": 4})["x"]->ignore

let zz = ({"_5": 3})["_5"]

type t = {
  @as("0123") a: int,
  @as("123_456") b: int,
}

let h = {a: 2, b: 3}
let f = id => {
  while false {
    ()
  }
  id
}

eq(__LOC__, ({"_5": 3})["_5"], 3)
eq(__LOC__, (2, 3), (f(h).a, f(h).b))
Js.log(Obj.tag(Obj.repr({"_5": 3})))

Mt.from_pair_suites(__LOC__, suites.contents)

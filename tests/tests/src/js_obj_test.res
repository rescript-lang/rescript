open Js_obj

type x = {"say": int => int}

let suites = {
  open Mt
  list{
    ("empty", _ => Eq(0, Belt.Array.length(keys(empty())))),
    ("assign", _ => Eq({"a": 1}, assign(empty(), {"a": 1}))),
  }
}

Mt.from_pair_suites(__MODULE__, suites)

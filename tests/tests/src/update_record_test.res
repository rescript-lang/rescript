let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  Js.log((x, y))
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Eq(x, y)),
      ...suites.contents,
    }
}

type t = {
  a0: int,
  a1: int,
  a2: int,
  a3: int,
  a4: int,
  a5: int,
  /* a6 : int ; */
  /* mutable a7 : int ; */
}

let f = (x: t) => {
  let y: t = Obj.magic(Obj.dup(Obj.repr(x)))
  {...y, a0: 1}
}

let () = {
  let v = {a0: 0, a1: 0, a2: 0, a3: 0, a4: 0, a5: 0}
  eq(__LOC__, v.a0 + 1, f(v).a0)
}

type t0 = {
  invalid_js_id': int,
  x: int,
}
let val0 = {invalid_js_id': 3, x: 2}
let fff = x => {...x, invalid_js_id': x.invalid_js_id' + 2}

let val1 = fff(val0)

let () = {
  eq(__LOC__, val0.invalid_js_id', 3)
  eq(__LOC__, val1.invalid_js_id', 5)
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)

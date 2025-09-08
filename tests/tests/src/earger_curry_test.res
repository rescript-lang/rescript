open Belt

let f = g => x => g(x)

let map = (f, a) => {
  let l = Array.length(a)
  if l == 0 {
    []
  } else {
    let r = Array.make(l, f(Array.getUnsafe(a, 0)))
    for i in 1 to l - 1 {
      Array.setUnsafe(r, i, f(Array.getUnsafe(a, i)))
    }
    r
  }
}

let map = (type u v, f: u => v, a: array<u>): array<v> => map(x => f(x), a)

let init = (l, f) =>
  if l == 0 {
    []
  } else if l < 0 {
    invalid_arg("Array.init")
  } else {
    /* See #6575. We could also check for maximum array size, but this depends
     on whether we create a float array or a regular one... */

    let res = Array.make(l, f(0))
    for i in 1 to pred(l) {
      Array.setUnsafe(res, i, f(i))
    }
    res
  }

let init = (l, f) => init(l, x => f(x))

let fold_left = (f, x, a) => {
  let r = ref(x)
  for i in 0 to Array.length(a) - 1 {
    r := f(r.contents, Array.getUnsafe(a, i))
  }
  r.contents
}

let fold_left = (f, x, a) => fold_left((x, y) => f(x, y), x, a)

@val external timeStart: string => unit = "console.time"

@val external timeEnd: string => unit = "console.timeEnd"

let f = {
  open Array
  () => {
    let arr = init(10000000, i => float_of_int(i))
    let b = arr->map(i => i +. i -. 1.)
    let v = b->reduceReverse(0., \"+.")
    v->Js.Float.toString->Js.log
  }
}

let f2 = () => {
  open Array
  let arr = init(30_000_000, i => float_of_int(i))
  let b = arr->map(i => i +. i -. 1.)
  let v = b->reduceReverse(0., \"+.")
  v->Js.Float.toString->Js.log
}

/* let time label f = */
/* timeStart label ; */
/* f (); */
/* timeEnd label */

/* ;; */
/* begin */
/* time "curried" f ; */
/* time "uncurried" f2; */
/* end */

let () = f2()

/* ocamlbuild */
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

let v = ref(0)

let all_v = ref(list{})

let add5 = (a0, a1, a2, a3, a4) => {
  /* [@bs.noinline] ; */ /* Makes sense for debugging */
  Js.log((a0, a1, a2, a3, a4))
  all_v := list{v.contents, ...all_v.contents}
  a0 + a1 + a2 + a3 + a4
}

let f = x =>
  /* let u = */ add5(
    x,
    {
      incr(v)
      1
    },
    {
      incr(v)
      2
    },
    ...
  ) /* in */
/* all_v := !v :: !all_v ;
 u */

let g = x => {
  let u = (a, b) =>
    add5(
      x,
      {
        incr(v)
        1
      },
      {
        incr(v)
        2
      },
      a,
      b,
    )
  all_v := list{v.contents, ...all_v.contents}
  u
}
let a = f(0)(3, 4)

let b = f(0)(3, 5)

let c = g(0)(3, 4)
let d = g(0)(3, 5)

let () = {
  eq(__LOC__, a, 10)
  eq(__LOC__, b, 11)
  eq(__LOC__, c, 10)
  eq(__LOC__, d, 11)
  eq(__LOC__, all_v.contents, list{8, 6, 6, 4, 4, 2})
}
let () = Mt.from_pair_suites(__MODULE__, suites.contents)

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

let called = ref(0)
/* function hoisting prevent the toplevel bug */
let g = () => {
  let rec v = ref(next)
  and next = (i, b) => {
    incr(called)
    if b {
      ignore(v.contents(i, false))
    }
    i + 1
  }

  Js.log(Js.Int.toString(next(0, true)))
}

g()

let rec x = list{1, ...y}
and y = list{2, ...x}

eq(__LOC__, called.contents, 2)

Mt.from_pair_suites(__MODULE__, suites.contents)

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

@send external map: (array<'a>, 'a => 'b) => array<'b> = "map"

%%raw(`
function hi (cb){
    cb ();
    return 0;
}
`)

@val external hi: (unit => unit) => unit = "hi"

let () = {
  let xs = ref(list{})
  hi((() as x) => xs := list{x, ...xs.contents})
  hi((() as x) => xs := list{x, ...xs.contents})
  eq(__LOC__, xs.contents, list{(), ()})
}

let () = {
  eq(__LOC__, [1, 2, 3]->map(x => x + 1), [2, 3, 4])
  eq(__LOC__, [1, 2, 3]->Js.Array2.map(x => x + 1), [2, 3, 4])

  eq(__LOC__, [1, 2, 3]->Js.Array2.reduce(\"+", 0), 6)

  eq(__LOC__, [1, 2, 3]->Js.Array2.reducei((x, y, i) => x + y + i, 0), 9)

  eq(__LOC__, [1, 2, 3]->Js.Array2.some(x => x < 1), false)

  eq(__LOC__, [1, 2, 3]->Js.Array2.every(x => x > 0), true)
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)

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

@deriving({jsConverter: newType})
type t<'a> = {
  x: int,
  y: bool,
  z: 'a,
}

let v0 = tToJs({x: 3, y: false, z: false})
let v1 = tToJs({x: 3, y: false, z: ""})

@deriving({jsConverter: newType})
type x = [
  | #a
  | #b
  | #c
]

let idx = v => eq(__LOC__, xFromJs(xToJs(v)), v)
let x0 = xToJs(#a)
let x1 = xToJs(#b)

let () = {
  idx(#a)
  idx(#b)
  idx(#c)
}

Mt.from_pair_suites(__MODULE__, suites.contents)

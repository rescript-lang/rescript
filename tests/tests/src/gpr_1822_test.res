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

type shape =
  | Circle(int)
  | Rectangle(int, int)

let myShape = Circle(10)
let area = switch myShape {
| Circle(r) => float_of_int(r * r) *. 3.14
| Rectangle(w, h) => float_of_int(w * h)
}

eq(__LOC__, area, 314.)

Mt.from_pair_suites(__MODULE__, suites.contents)

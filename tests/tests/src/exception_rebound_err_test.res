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

exception A(int)
exception B
exception C(int, int)

let test_js_error4 = () =>
  try {
    ignore(Js.Json.parseExn(` {"x"}`))
    1
  } catch {
  | Not_found => 2
  | Invalid_argument("x") => 3
  | A(2) => 4
  | B => 5
  | C(1, 2) => 6
  | e => 7
  }

let f = g =>
  try g() catch {
  | Not_found => 1
  }

eq(__LOC__, test_js_error4(), 7)
Mt.from_pair_suites(__MODULE__, suites.contents)

/* ;; if bool_of_string "x" then "" else "" */

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

let v = ref(3)

let update = () => {
  incr(v)
  true
}

if update() {
  ""
} else {
  ""
}->ignore

eq(__LOC__, v.contents, 4)

Mt.from_pair_suites(__MODULE__, suites.contents)

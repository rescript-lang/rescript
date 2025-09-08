@@config(no_export)

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)

let {eq_suites} = module(Mt)
let eq = (loc, x, y) => eq_suites(loc, x, y, ~test_id, ~suites)

let then = (a, b) => {
  Js.log("no inline")
  a * a + b * b
}

eq(__LOC__, then(1, 2), 5)

Mt.from_pair_suites(__FILE__, suites.contents)

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

let f = (~x=3, y: int) => {
  let xOpt = x + 2
  Js.log(xOpt)
  xOpt + y
}

Js.log(f(2))

eq(__LOC__, f(2), 7)

eq(__LOC__, f(~x=4, 2), 8)

Mt.from_pair_suites(__FILE__, suites.contents)

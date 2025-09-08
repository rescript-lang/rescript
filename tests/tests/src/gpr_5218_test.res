let {eq_suites} = module(Mt)
let test_id = ref(0)
let suites = ref(list{})

let test = x =>
  switch x {
  | #1(x) => #1(x)
  | #2(x) => #2(x)
  }

eq_suites(~test_id, ~suites, __LOC__, test(#1(3)), #1(3))

eq_suites(~test_id, ~suites, __LOC__, test(#2(3)), #2(3))

Mt.from_pair_suites(__FILE__, suites.contents)

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

type collision = [#Eric_Cooper | #azdwbie]

let f0 = x =>
  switch x {
  | #Eric_Cooper => 0
  | #azdwbie => 1
  }

let f1 = x =>
  switch x {
  | #Eric_Cooper(x) => x + 1
  | #azdwbie(x) => x + 2
  }

let hi: array<collision> = [#Eric_Cooper, #azdwbie]
eq(__LOC__, f0(#Eric_Cooper), 0)
eq(__LOC__, f0(#azdwbie), 1)

eq(__LOC__, f1(#Eric_Cooper(-1)), 0)

eq(__LOC__, f1(#azdwbie(-2)), 0)

Mt.from_pair_suites(__FILE__, suites.contents)

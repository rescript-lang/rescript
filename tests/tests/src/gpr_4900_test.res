let {from_pair_suites, eq_suites} = module(Mt)

let suites = ref(list{})
let test_id = ref(0)
type show = No | After(int) | Yes

let showToJs = x =>
  switch x {
  | Yes | After(_) => true
  | No => false
  }

eq_suites(~test_id, ~suites, __LOC__, showToJs(Yes), true)
eq_suites(~test_id, ~suites, __LOC__, showToJs(No), false)
eq_suites(~test_id, ~suites, __LOC__, showToJs(After(3)), true)

from_pair_suites(__LOC__, suites.contents)

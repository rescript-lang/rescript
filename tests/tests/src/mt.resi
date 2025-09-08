type rec eq =
  | Eq('a, 'a): eq
  | Neq('a, 'a): eq
  | StrictEq('a, 'a): eq
  | StrictNeq('a, 'a): eq
  | Ok(bool): eq
  | Approx(float, float): eq
  | ApproxThreshold(float, float, float): eq
  | ThrowAny(unit => unit): eq
  | Fail(unit): eq
  | FailWith(string): eq
type pair_suites = list<(string, unit => eq)>

let from_suites: (string, list<(string, unit => unit)>) => unit
let from_pair_suites: (string, pair_suites) => unit

type promise_suites = list<(string, Js.Promise.t<eq>)>

let from_promise_suites: (string, promise_suites) => unit

let old_from_promise_suites_donotuse: (string, promise_suites) => unit

let eq_suites: (~test_id: ref<int>, ~suites: ref<pair_suites>, string, 'b, 'b) => unit

let bool_suites: (~test_id: ref<int>, ~suites: ref<pair_suites>, string, bool) => unit

let throw_suites: (~test_id: ref<int>, ~suites: ref<pair_suites>, string, unit => unit) => unit

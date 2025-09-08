let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

let fake_c2 = (a_type, b_type) =>
  switch (a_type, b_type) {
  | ("undefined", _) => -1
  | (_, "undefined") => 1
  | ("string", _) => 1
  | ("number", "number") => 33
  | ("number", _) => 3
  | _ => 0
  }

eq(__LOC__, 3, fake_c2("number", "xx"))
Mt.from_pair_suites(__MODULE__, suites.contents)

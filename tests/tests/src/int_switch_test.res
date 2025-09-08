let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(loc, x, y, ~test_id, ~suites)
let b = (loc, x) => Mt.bool_suites(loc, x, ~test_id, ~suites)

let f = x =>
  switch x() {
  | 1 => 'a'
  | 2 => 'b'
  | 3 => 'c'
  | _ => 'x'
  }

type t = A | B | C | D

let f22 = x =>
  switch x() {
  | 3 => 'c'
  | 2 => 'b'
  | 1 => 'a'
  | _ => 'x'
  }

let f33 = x =>
  switch x() {
  | C => 'c'
  | B => 'b'
  | A => 'a'
  | _ => 'x'
  }

eq(__LOC__, f(_ => 1), 'a')
eq(__LOC__, f(_ => 2), 'b')
eq(__LOC__, f(_ => 3), 'c')
eq(__LOC__, f(_ => 0), 'x')
eq(__LOC__, f(_ => -1), 'x')

Mt.from_pair_suites(__MODULE__, suites.contents)

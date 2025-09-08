let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

let u = 0032 /* integer formatting */

let x = 00007.0
let y = 02e3
let z = 0000.2
let others = (00., 000., 01., 001., 002e3)

let () = {
  eq(__LOC__, x, 7.)
  eq(__LOC__, y, 2000.)
  eq(__LOC__, z, 0.2)
  eq(__LOC__, u, 32)
  eq(__LOC__, others, (0.0, 0.0, 1.0, 1.0, 2e3))
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)

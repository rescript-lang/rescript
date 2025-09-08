let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

let non_debug_u = Debug_mode_value.A(1, 2)

eq(__LOC__, Debug_mode_value.u, non_debug_u)
Mt.from_pair_suites(__LOC__, suites.contents)

let suites: ref<Mt.pair_suites> = ref(list{})

let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

module H = Gpr_3566_test.Test()
let () = eq(__LOC__, H.b, true)

module Caml_option = {}
let f = x => Some(x)
let () = Mt.from_pair_suites(__FILE__, suites.contents)

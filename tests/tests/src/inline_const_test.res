let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

module H = Inline_const.N1()

let (f, f1, f2, f3, f4, f5, f6, f7) = {
  open Inline_const
  (f, f1, f2, N.f3, H.f4, f5, f6, H.xx)
}

let () = {
  eq(__LOC__, f, "hello")
  eq(__LOC__, f1, "a")
  eq(__LOC__, f2, `中文`)
  eq(__LOC__, f3, `中文`)
  eq(__LOC__, f4, `中文`)
  eq(__LOC__, f5, true)
  eq(__LOC__, f6, 1)
  eq(__LOC__, f7, 0.000003)
}

let () = Mt.from_pair_suites(__LOC__, suites.contents)

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

external jsPow: (float, float) => float = "Math.pow"

let intPow: (int, int) => int = %raw(`(a, b) => Math.pow(a, b) | 0`)
let four: int = %raw(`4`)

let () = {
  eq(__LOC__, 2. ** 3. ** 2., jsPow(2., jsPow(3., 2.)))
  eq(__LOC__, 2. ** -3. ** 2., jsPow(2., jsPow(-3., 2.)))
  eq(__LOC__, (2. ** 3.) ** 2., jsPow(jsPow(2., 3.), 2.))
  eq(__LOC__, -2. ** 2., jsPow(-2., 2.))

  eq(__LOC__, 2 ** 3 ** 2, intPow(2, intPow(3, 2)))
  eq(__LOC__, 2 ** -3 ** 2, intPow(2, intPow(-3, 2)))
  eq(__LOC__, (2 ** 3) ** 2, intPow(intPow(2, 3), 2))
  eq(__LOC__, -2 ** 31, intPow(-2, 31))
  eq(__LOC__, 2 ** 32, intPow(2, 32))
  eq(__LOC__, 2147483647 ** 2, intPow(2147483647, 2))
  eq(__LOC__, -2147483648 ** 2, intPow(-2147483648, 2))

  eq(__LOC__, 4 ** 4, four ** four)
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)

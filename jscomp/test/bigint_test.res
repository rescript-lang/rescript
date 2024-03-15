let (test_id, suites) = (ref(0), ref(list{}))
let eq = loc => Mt_global.collect_eq(test_id, suites, loc)
let approx = loc => Mt_global.collect_approx(test_id, suites, loc)

let bigint_compare = (x: bigint, y) => Pervasives.compare(x, y)
let generic_compare = Pervasives.compare
let bigint_equal = (x: bigint, y) => x == y
let generic_equal = \"="
let bigint_notequal = (x: bigint, y) => x != y
let generic_notequal = \"<>"
let bigint_lessthan = (x: bigint, y) => x < y
let generic_lessthan = \"<"
let bigint_greaterthan = (x: bigint, y) => x > y
let generic_greaterthan = \">"
let bigint_lessequal = (x: bigint, y) => x <= y
let generic_lessequal = \"<="
let bigint_greaterequal = (x: bigint, y) => x >= y
let generic_greaterequal = \">="

let () = {
  eq(__LOC__, bigint_compare(1n, 1n), 0)
  eq(__LOC__, generic_compare(1n, 1n), 0)
  eq(__LOC__, bigint_compare(-0n, -1n), 1)
  eq(__LOC__, generic_compare(-0n, -1n), 1)
  eq(__LOC__, bigint_compare(+0n, -1n), 1)
  eq(__LOC__, generic_compare(+0n, -1n), 1)
  eq(__LOC__, bigint_compare(1n, 2n), -1)
  eq(__LOC__, generic_compare(1n, 2n), -1)
  eq(__LOC__, bigint_equal(1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n, 1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n), true)
  eq(__LOC__, generic_equal(1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n, 1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n), true)
  eq(__LOC__, bigint_equal(1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n, 1000000000000000000000000000000000000000000000000000000000000000000000000000000000001n), false)
  eq(__LOC__, generic_equal(1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n, 1000000000000000000000000000000000000000000000000000000000000000000000000000000000001n), false)
  eq(__LOC__, bigint_equal(1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n, -1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n), false)
  eq(__LOC__, generic_equal(1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n, -1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n), false)
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)

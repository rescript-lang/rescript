let (test_id, suites) = (ref(0), ref(list{}))
let eq = (loc, x, y) => Mt_global.collect_eq(test_id, suites, loc, x, y)
let approx = (loc, x, y) => Mt_global.collect_approx(test_id, suites, loc, x, y)

let bigint_compare = (x: bigint, y) => Pervasives.compare(x, y)
let generic_compare = Pervasives.compare
let bigint_equal = (x: bigint, y) => x == y
let generic_equal = \"=="
let bigint_notequal = (x: bigint, y) => x != y
let generic_notequal = \"!="
let bigint_lessthan = (x: bigint, y) => x < y
let generic_lessthan = \"<"
let bigint_greaterthan = (x: bigint, y) => x > y
let generic_greaterthan = \">"
let bigint_lessequal = (x: bigint, y) => x <= y
let generic_lessequal = \"<="
let bigint_greaterequal = (x: bigint, y) => x >= y
let generic_greaterequal = \">="
let bigint_land = Js.BigInt.land
let bigint_lor = Js.BigInt.lor
let bigint_lxor = Js.BigInt.lxor
let bigint_lsl = Js.BigInt.lsl
let bigint_asr = Js.BigInt.asr

let () = {
  eq(__LOC__, bigint_compare(1n, 1n), 0)
  eq(__LOC__, generic_compare(1n, 1n), 0)
  eq(__LOC__, bigint_compare(-0n, -1n), 1)
  eq(__LOC__, generic_compare(-0n, -1n), 1)
  eq(__LOC__, bigint_compare(0n, -1n), 1)
  eq(__LOC__, generic_compare(0n, -1n), 1)
  eq(__LOC__, bigint_compare(1n, 2n), -1)
  eq(__LOC__, generic_compare(1n, 2n), -1)
  eq(__LOC__, bigint_compare(1n, 02n), -1)
  eq(__LOC__, generic_compare(1n, 02n), -1)
  eq(__LOC__, bigint_compare(1n, 001n), 0)
  eq(__LOC__, generic_compare(1n, 001n), 0)
  eq(
    __LOC__,
    bigint_equal(
      1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n,
      1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n,
    ),
    true,
  )
  eq(
    __LOC__,
    generic_equal(
      1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n,
      1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n,
    ),
    true,
  )
  eq(
    __LOC__,
    bigint_equal(
      1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n,
      1000000000000000000000000000000000000000000000000000000000000000000000000000000000001n,
    ),
    false,
  )
  eq(
    __LOC__,
    generic_equal(
      1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n,
      1000000000000000000000000000000000000000000000000000000000000000000000000000000000001n,
    ),
    false,
  )
  eq(
    __LOC__,
    bigint_equal(
      1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n,
      -1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n,
    ),
    false,
  )
  eq(
    __LOC__,
    generic_equal(
      1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n,
      -1000000000000000000000000000000000000000000000000000000000000000000000000000000000000n,
    ),
    false,
  )
  eq(
    __LOC__,
    bigint_equal(
      switch 1n {
      | 1n => 3n
      | 2n => 4n
      | _ => 0n
      },
      3n,
    ),
    true,
  )
  eq(
    __LOC__,
    generic_equal(
      switch 1n {
      | 1n => 3n
      | 2n => 4n
      | _ => 0n
      },
      3n,
    ),
    true,
  )
  eq(
    __LOC__,
    bigint_equal(
      switch -1n {
      | -00001n => 3n
      | -2n => 4n
      | _ => 0n
      },
      3n,
    ),
    true,
  )
  eq(
    __LOC__,
    generic_equal(
      switch -1n {
      | -00001n => 3n
      | 2n => 4n
      | _ => 0n
      },
      3n,
    ),
    true,
  )
  eq(
    __LOC__,
    bigint_equal(
      switch -0001_000n {
      | -00001000n => 3n
      | -0002n => 4n
      | _ => 0n
      },
      3n,
    ),
    true,
  )
  eq(
    __LOC__,
    generic_equal(
      switch -0001_000n {
      | -00001000n => 3n
      | -0002n => 4n
      | _ => 0n
      },
      3n,
    ),
    true,
  )
  eq(__LOC__, bigint_land(9n, 1n), 1n)
  eq(__LOC__, bigint_lor(9n, 1n), 9n)
  eq(__LOC__, bigint_lxor(9n, 1n), 8n)
  eq(__LOC__, bigint_lsl(9n, 1n), 18n)
  eq(__LOC__, bigint_lsl(9n, -1n), 4n)
  eq(__LOC__, bigint_asr(9n, 1n), 4n)
  eq(__LOC__, bigint_asr(9n, -1n), 18n)
  eq(__LOC__, bigint_asr(-9n, 1n), -5n)
  eq(__LOC__, bigint_asr(-9n, -1n), -18n)
  eq(__LOC__, max(9n, 1n), 9n)
  eq(__LOC__, min(9n, 1n), 1n)
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)

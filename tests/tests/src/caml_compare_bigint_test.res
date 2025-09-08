@val external bigint: string => float = "BigInt"

let isLessThan = (title, small, big) => list{
  ("compare: " ++ title, _ => Mt.Eq(true, compare(big, small) > 0)),
  ("compare: " ++ title, _ => Mt.Eq(true, compare(small, big) < 0)),
  ("< operator: " ++ title, _ => Mt.Eq(true, small < big)),
  ("<= operator: " ++ title, _ => Mt.Eq(true, small <= big)),
  ("> operator: " ++ title, _ => Mt.Eq(true, big > small)),
  (">= operator: " ++ title, _ => Mt.Eq(true, big >= small)),
  ("min: " ++ title, _ => Mt.Eq(small, min(big, small))),
  ("min: " ++ title, _ => Mt.Eq(small, min(small, big))),
  ("max: " ++ title, _ => Mt.Eq(big, max(big, small))),
  ("max: " ++ title, _ => Mt.Eq(big, max(small, big))),
  ("!== operator: " ++ title, _ => Mt.Eq(true, big !== small)),
  ("!== operator: " ++ title, _ => Mt.Eq(true, small !== big)),
  ("!= operator: " ++ title, _ => Mt.Eq(true, big != small)),
  ("!= operator: " ++ title, _ => Mt.Eq(true, small != big)),
  ("== operator: " ++ title, _ => Mt.Eq(false, big == small)),
  ("== operator: " ++ title, _ => Mt.Eq(false, small == big)),
  ("=== operator: " ++ title, _ => Mt.Eq(false, big === small)),
  ("=== operator: " ++ title, _ => Mt.Eq(false, small === big)),
}

let isEqual = (title, num1, num2) => list{
  ("< operator: " ++ title, _ => Mt.Eq(false, num2 < num1)),
  ("<= operator: " ++ title, _ => Mt.Eq(true, num2 <= num1)),
  ("> operator: " ++ title, _ => Mt.Eq(false, num1 > num2)),
  (">= operator: " ++ title, _ => Mt.Eq(true, num1 >= num2)),
  ("min: " ++ title, _ => Mt.Eq(num1, min(num1, num2))),
  ("max: " ++ title, _ => Mt.Eq(num1, max(num1, num2))),
  ("compare: " ++ title, _ => Mt.Eq(0, compare(num1, num2))),
  ("compare: " ++ title, _ => Mt.Eq(0, compare(num2, num1))),
  ("!= operator: " ++ title, _ => Mt.Eq(false, num1 !== num2)),
  ("!= operator: " ++ title, _ => Mt.Eq(false, num2 !== num1)),
  ("!= operator: " ++ title, _ => Mt.Eq(false, num1 != num2)),
  ("!= operator: " ++ title, _ => Mt.Eq(false, num2 != num1)),
  ("== operator: " ++ title, _ => Mt.Eq(true, num1 == num2)),
  ("== operator: " ++ title, _ => Mt.Eq(true, num2 == num1)),
  ("=== operator: " ++ title, _ => Mt.Eq(true, num1 === num2)),
  ("=== operator: " ++ title, _ => Mt.Eq(true, num2 === num1)),
}

let five = bigint("5")

/** Not comparing floats and BigInt; not sure this is correct since works in JavaScript */
let suites: Mt.pair_suites = \"@"(
  isLessThan("123 and 555555", bigint("123"), bigint("555555")),
  \"@"(
    isEqual("98765 and 98765", bigint("98765"), bigint("98765")),
    isEqual("same instance", five, five),
  ),
)

let () = Mt.from_pair_suites(__FILE__, suites)

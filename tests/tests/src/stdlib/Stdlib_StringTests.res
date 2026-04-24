let eq = (a, b) => a == b

Test.run(__POS_OF__("String.equal optimization"), String.equal("one", "three"), eq, false)
Test.run(
  __POS_OF__("String.asIterable"),
  "abc"->String.asIterable->Array.fromIterable,
  eq,
  ["a", "b", "c"],
)

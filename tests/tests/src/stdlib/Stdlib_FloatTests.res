let eq = (a, b) => a == b

Test.run(__POS_OF__("clamp"), Float.clamp(4.2), eq, 4.2)
Test.run(__POS_OF__("clamp - < min"), Float.clamp(~min=4.3, 4.1), eq, 4.3)
Test.run(__POS_OF__("clamp - > min"), Float.clamp(~min=4.1, 4.2), eq, 4.2)
Test.run(__POS_OF__("clamp - < max"), Float.clamp(~max=4.3, 4.2), eq, 4.2)
Test.run(__POS_OF__("clamp - > max"), Float.clamp(~max=4.1, 4.2), eq, 4.1)
Test.run(__POS_OF__("clamp - < min, < max"), Float.clamp(~min=4.3, ~max=4.5, 4.2), eq, 4.3)
Test.run(__POS_OF__("clamp - < min, > max"), Float.clamp(~min=4.3, ~max=4.1, 4.2), eq, 4.3) // min wins
Test.run(__POS_OF__("clamp - > min, < max"), Float.clamp(~min=4.1, ~max=4.5, 4.2), eq, 4.2)
Test.run(__POS_OF__("clamp - > min, > max"), Float.clamp(~min=4.1, ~max=4.1, 4.2), eq, 4.1)
Test.run(__POS_OF__("clamp - nan"), Float.clamp(~min=4.1, ~max=4.3, nan)->Float.isNaN, eq, true)
Test.run(__POS_OF__("clamp - infinity"), Float.clamp(~min=4.1, ~max=4.3, infinity), eq, 4.3)
Test.run(__POS_OF__("clamp - -infinity"), Float.clamp(~min=4.1, ~max=4.3, neg_infinity), eq, 4.1)
Test.run(__POS_OF__("clamp - min nan"), Float.clamp(~min=nan, 4.2), eq, 4.2)
Test.run(__POS_OF__("clamp - max nan"), Float.clamp(~max=nan, 4.2), eq, 4.2)
Test.run(__POS_OF__("clamp - min nan, max nan"), Float.clamp(~min=nan, ~max=nan, 4.2), eq, 4.2)
Test.run(__POS_OF__("clamp - min infinity"), Float.clamp(~min=infinity, 4.2), eq, infinity)
Test.run(__POS_OF__("clamp - max infinity"), Float.clamp(~max=infinity, 4.2), eq, 4.2)
Test.run(__POS_OF__("clamp - min -infinity"), Float.clamp(~min=neg_infinity, 4.2), eq, 4.2)
Test.run(__POS_OF__("clamp - max -infinity"), Float.clamp(~max=neg_infinity, 4.2), eq, neg_infinity)
Test.run(
  __POS_OF__("clamp - min infinity, max infinity"),
  Float.clamp(~min=infinity, ~max=infinity, 4.2),
  eq,
  infinity,
)
Test.run(
  __POS_OF__("clamp - min -infinity, max infinity"),
  Float.clamp(~min=neg_infinity, ~max=infinity, 4.2),
  eq,
  4.2,
)
Test.run(
  __POS_OF__("clamp - min infinity, max -infinity"),
  Float.clamp(~min=infinity, ~max=neg_infinity, 4.2),
  eq,
  infinity, // min wins
)
Test.run(
  __POS_OF__("clamp - min -infinity, max -infinity"),
  Float.clamp(~min=neg_infinity, ~max=neg_infinity, 4.2),
  eq,
  neg_infinity,
)

Test.run(__POS_OF__("Float.equal optimization"), Float.equal(1., 3.), eq, false)

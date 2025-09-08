let eq = (a, b) => a == b

// Test for RegExp.flags
Test.run(
  __POS_OF__("RegExp.flags basic"),
  RegExp.fromStringWithFlags("\\w+", ~flags="gi")->RegExp.flags,
  eq,
  "gi",
)

// Test for alphabetical sorting of flags
Test.run(
  __POS_OF__("RegExp.flags sorting"),
  RegExp.fromStringWithFlags("\\w+", ~flags="igd")->RegExp.flags,
  eq,
  "dgi",
)

// Test with no flags
Test.run(__POS_OF__("RegExp.flags empty"), RegExp.fromString("\\w+")->RegExp.flags, eq, "")

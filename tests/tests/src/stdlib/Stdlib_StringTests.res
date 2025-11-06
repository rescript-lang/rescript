let eq = (a, b) => a == b

Test.run(__POS_OF__("String.equal optimization"), String.equal("one", "three"), eq, false)

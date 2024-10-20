// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mocha = require("mocha");
let Belt_Int = require("rescript/lib/js/Belt_Int.js");
let Test_utils = require("./test_utils.js");

Mocha.describe("Belt_int_test", () => {
  Mocha.test("toFloat", () => {
    Test_utils.eq("File \"belt_int_test.res\", line 8, characters 7-14", 1, 1.0);
    Test_utils.eq("File \"belt_int_test.res\", line 9, characters 7-14", -1, -1.0);
  });
  Mocha.test("fromFloat", () => {
    Test_utils.eq("File \"belt_int_test.res\", line 13, characters 7-14", 1, 1);
    Test_utils.eq("File \"belt_int_test.res\", line 14, characters 7-14", 1, 1);
    Test_utils.eq("File \"belt_int_test.res\", line 15, characters 7-14", 1, 1);
    Test_utils.eq("File \"belt_int_test.res\", line 16, characters 7-14", -1, -1);
    Test_utils.eq("File \"belt_int_test.res\", line 17, characters 7-14", -1, -1);
    Test_utils.eq("File \"belt_int_test.res\", line 18, characters 7-14", -1, -1);
  });
  Mocha.test("fromString", () => {
    Test_utils.eq("File \"belt_int_test.res\", line 22, characters 7-14", Belt_Int.fromString("1"), 1);
    Test_utils.eq("File \"belt_int_test.res\", line 23, characters 7-14", Belt_Int.fromString("-1"), -1);
    Test_utils.eq("File \"belt_int_test.res\", line 24, characters 7-14", Belt_Int.fromString("1.7"), 1);
    Test_utils.eq("File \"belt_int_test.res\", line 25, characters 7-14", Belt_Int.fromString("-1.0"), -1);
    Test_utils.eq("File \"belt_int_test.res\", line 26, characters 7-14", Belt_Int.fromString("-1.5"), -1);
    Test_utils.eq("File \"belt_int_test.res\", line 27, characters 7-14", Belt_Int.fromString("-1.7"), -1);
    Test_utils.eq("File \"belt_int_test.res\", line 28, characters 7-14", Belt_Int.fromString("not an int"), undefined);
  });
  Mocha.test("toString", () => {
    Test_utils.eq("File \"belt_int_test.res\", line 32, characters 7-14", String(1), "1");
    Test_utils.eq("File \"belt_int_test.res\", line 33, characters 7-14", String(-1), "-1");
  });
  Mocha.test("operators", () => {
    Test_utils.eq("File \"belt_int_test.res\", line 39, characters 7-14", 5, 5);
    Test_utils.eq("File \"belt_int_test.res\", line 40, characters 7-14", -1, -1);
    Test_utils.eq("File \"belt_int_test.res\", line 41, characters 7-14", 6, 6);
    Test_utils.eq("File \"belt_int_test.res\", line 42, characters 7-14", 0, 0);
  });
});

let I;

exports.I = I;
/*  Not a pure module */

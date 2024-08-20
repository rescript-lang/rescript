// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Nodetest = require("node:test");
let Belt_Float = require("../../lib/js/belt_Float.js");
let Nodeassert = require("node:assert");

function eq(loc, a, b) {
  Nodeassert.strictEqual(a, b, loc);
}

Nodetest.describe("Belt.Float", () => {
  Nodetest.test("fromInt", () => {
    eq("File \"belt_float_ntest.res\", line 9, characters 7-14", 1, 1.0);
    eq("File \"belt_float_ntest.res\", line 10, characters 7-14", -1, -1.0);
  });
  Nodetest.test("toInt", () => {
    eq("File \"belt_float_ntest.res\", line 14, characters 7-14", 1, 1);
    eq("File \"belt_float_ntest.res\", line 15, characters 7-14", 1, 1);
    eq("File \"belt_float_ntest.res\", line 16, characters 7-14", 1, 1);
    eq("File \"belt_float_ntest.res\", line 17, characters 7-14", -1, -1);
    eq("File \"belt_float_ntest.res\", line 18, characters 7-14", -1, -1);
    eq("File \"belt_float_ntest.res\", line 19, characters 7-14", -1, -1);
  });
  Nodetest.test("fromString", () => {
    eq("File \"belt_float_ntest.res\", line 23, characters 7-14", Belt_Float.fromString("1"), 1.0);
    eq("File \"belt_float_ntest.res\", line 24, characters 7-14", Belt_Float.fromString("-1"), -1.0);
    eq("File \"belt_float_ntest.res\", line 25, characters 7-14", Belt_Float.fromString("1.7"), 1.7);
    eq("File \"belt_float_ntest.res\", line 26, characters 7-14", Belt_Float.fromString("-1.0"), -1.0);
    eq("File \"belt_float_ntest.res\", line 27, characters 7-14", Belt_Float.fromString("-1.5"), -1.5);
    eq("File \"belt_float_ntest.res\", line 28, characters 7-14", Belt_Float.fromString("-1.7"), -1.7);
    eq("File \"belt_float_ntest.res\", line 29, characters 7-14", Belt_Float.fromString("not a float"), undefined);
  });
  Nodetest.test("toString", () => {
    eq("File \"belt_float_ntest.res\", line 33, characters 7-14", String(1.0), "1");
    eq("File \"belt_float_ntest.res\", line 34, characters 7-14", String(-1.0), "-1");
    eq("File \"belt_float_ntest.res\", line 35, characters 7-14", String(-1.5), "-1.5");
  });
  Nodetest.test("operators", () => {
    eq("File \"belt_float_ntest.res\", line 40, characters 7-14", 2.0 + 3.0, 5.0);
    eq("File \"belt_float_ntest.res\", line 41, characters 7-14", 2.0 - 3.0, -1.0);
    eq("File \"belt_float_ntest.res\", line 42, characters 7-14", 2.0 * 3.0, 6.0);
    eq("File \"belt_float_ntest.res\", line 43, characters 7-14", 3.0 / 2.0, 1.5);
  });
});

let F;

exports.F = F;
exports.eq = eq;
/*  Not a pure module */

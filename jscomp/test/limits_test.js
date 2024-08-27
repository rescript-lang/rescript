// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Int32 = require("../../lib/js/int32.js");
let Pervasives = require("../../lib/js/pervasives.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + test_id.contents.toString()),
      () => ({
        TAG: "Eq",
        _0: x,
        _1: y
      })
    ],
    tl: suites.contents
  };
}

eq("File \"limits_test.res\", line 10, characters 5-12", Pervasives.max_int, 2147483647);

eq("File \"limits_test.res\", line 11, characters 5-12", Pervasives.min_int, -2147483648);

eq("File \"limits_test.res\", line 12, characters 5-12", Int32.max_int, 2147483647);

eq("File \"limits_test.res\", line 13, characters 5-12", Int32.min_int, -2147483648);

Mt.from_pair_suites("Limits_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */

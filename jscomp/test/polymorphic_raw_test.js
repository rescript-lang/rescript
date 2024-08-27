// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");

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

let f = ((a) => typeof a);

let a = f(3);

let b = f("3");

eq("File \"polymorphic_raw_test.res\", line 18, characters 3-10", a, "number");

eq("File \"polymorphic_raw_test.res\", line 19, characters 3-10", b, "string");

Mt.from_pair_suites("polymorphic_raw_test.res", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.a = a;
exports.b = b;
/* a Not a pure module */

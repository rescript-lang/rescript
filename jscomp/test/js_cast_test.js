// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");

let suites = {
  contents: /* [] */0
};

let counter = {
  contents: 0
};

function add_test(loc, test) {
  counter.contents = counter.contents + 1 | 0;
  let id = loc + (" id " + counter.contents.toString());
  suites.contents = {
    hd: [
      id,
      test
    ],
    tl: suites.contents
  };
}

function eq(loc, x, y) {
  add_test(loc, () => ({
    TAG: "Eq",
    _0: x,
    _1: y
  }));
}

eq("File \"js_cast_test.res\", line 14, characters 12-19", true, 1);

eq("File \"js_cast_test.res\", line 16, characters 12-19", false, 0);

eq("File \"js_cast_test.res\", line 18, characters 12-19", 0, 0.0);

eq("File \"js_cast_test.res\", line 20, characters 12-19", 1, 1.0);

eq("File \"js_cast_test.res\", line 22, characters 12-19", 123456789, 123456789.0);

Mt.from_pair_suites("Js_cast_test", suites.contents);

exports.suites = suites;
exports.add_test = add_test;
exports.eq = eq;
/*  Not a pure module */

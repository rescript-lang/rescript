// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Pervasives = require("../../lib/js/pervasives.js");

function ff(v) {
  return v.toString();
}

function f(v) {
  return v.toString();
}

Mt.from_pair_suites("To_string_test", {
  hd: [
    "File \"to_string_test.res\", line 6, characters 8-15",
    () => ({
      TAG: "Eq",
      _0: Pervasives.infinity.toString(),
      _1: "Infinity"
    })
  ],
  tl: {
    hd: [
      "File \"to_string_test.res\", line 6, characters 54-61",
      () => ({
        TAG: "Eq",
        _0: Pervasives.neg_infinity.toString(),
        _1: "-Infinity"
      })
    ],
    tl: /* [] */0
  }
});

exports.ff = ff;
exports.f = f;
/*  Not a pure module */

// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let $$String = require("rescript/lib/js/String.js");

function u(v) {
  return v;
}

let s = $$String;

let N = {
  s: s
};

let v0 = "x".length;

function v(x) {
  return x.length;
}

let suites_0 = [
  "const",
  param => ({
    TAG: "Eq",
    _0: 1,
    _1: v0
  })
];

let suites_1 = {
  hd: [
    "other",
    param => ({
      TAG: "Eq",
      _0: 3,
      _1: v("abc")
    })
  ],
  tl: /* [] */0
};

let suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Module_parameter_test", suites);

exports.u = u;
exports.N = N;
exports.v0 = v0;
exports.v = v;
exports.suites = suites;
/*  Not a pure module */

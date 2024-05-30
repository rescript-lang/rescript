// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Caml = require("../../lib/js/caml.js");
let Caml_obj = require("../../lib/js/caml_obj.js");
let Caml_int64 = require("../../lib/js/caml_int64.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(param, param$1) {
  return Mt.bool_suites(test_id, suites, param, param$1);
}

function f(x, y) {
  return Caml.int_compare(x + y | 0, y + x | 0);
}

function f2(x, y) {
  return Caml.int_compare(x + y | 0, y);
}

let f3 = Caml.int_compare;

function f4(x, y) {
  if (x < y) {
    return x;
  } else {
    return y;
  }
}

let f5_min = Caml_obj.min;

let f5_max = Caml_obj.max;

b("File \"bs_min_max_test.res\", line 19, characters 4-11", Caml.i64_eq(Caml.i64_min(Caml_int64.zero, Caml_int64.one), Caml_int64.zero));

b("File \"bs_min_max_test.res\", line 20, characters 4-11", Caml.i64_eq(Caml.i64_max([
  0,
  22
], Caml_int64.one), [
  0,
  22
]));

b("File \"bs_min_max_test.res\", line 21, characters 4-11", Caml.i64_eq(Caml.i64_max([
  -1,
  4294967293
], [
  0,
  3
]), [
  0,
  3
]));

eq("File \"bs_min_max_test.res\", line 22, characters 5-12", Caml_obj.min(undefined, 3), undefined);

eq("File \"bs_min_max_test.res\", line 23, characters 5-12", Caml_obj.min(3, undefined), undefined);

eq("File \"bs_min_max_test.res\", line 24, characters 5-12", Caml_obj.max(3, undefined), 3);

eq("File \"bs_min_max_test.res\", line 25, characters 5-12", Caml_obj.max(undefined, 3), 3);

b("File \"bs_min_max_test.res\", line 26, characters 4-11", Caml_obj.greaterequal(5, undefined));

b("File \"bs_min_max_test.res\", line 27, characters 4-11", Caml_obj.lessequal(undefined, 5));

b("File \"bs_min_max_test.res\", line 28, characters 4-11", true);

b("File \"bs_min_max_test.res\", line 29, characters 4-11", true);

Mt.from_pair_suites("Bs_min_max_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.f = f;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5_min = f5_min;
exports.f5_max = f5_max;
/*  Not a pure module */

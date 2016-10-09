// Generated by BUCKLESCRIPT VERSION 1.2.1 , PLEASE EDIT WITH CARE
'use strict';

var Curry   = require("bs-platform/lib/js/curry");
var $$Array = require("bs-platform/lib/js/array");

var M = /* module */[];

function do_if(b) {
  if (b) {
    return /* true */1;
  }
  else {
    return /* false */0;
  }
}

function mk_tuple(int1, int2, _) {
  return /* tuple */[
          int1 + 1 | 0,
          int2 + 2 | 0,
          "3",
          /* "4" */52
        ];
}

function mk_rec(o) {
  return /* record */[
          /* name */Curry.js1(-922783157, 1, o),
          /* id */Curry.js1(23515, 2, o)
        ];
}

function mk_obj() {
  return {
          name: "nam",
          id: 2
        };
}

function mk_arr() {
  return /* int array */[
          1,
          2,
          3
        ];
}

function from_util() {
  return /* () */0;
}

function iter(items1, items2, fn1, fn2) {
  $$Array.iter(fn1, items1);
  return $$Array.iter(fn2, items2);
}

var M1 = 0;

var float_ = 1.0;

var tt = 0;

exports.M         = M;
exports.M1        = M1;
exports.do_if     = do_if;
exports.mk_tuple  = mk_tuple;
exports.mk_rec    = mk_rec;
exports.mk_obj    = mk_obj;
exports.mk_arr    = mk_arr;
exports.from_util = from_util;
exports.float_    = float_;
exports.tt        = tt;
exports.iter      = iter;
/* No side effect */

// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Caml = require("../../lib/js/caml.js");

function f(param, v) {
  return ((((param.x0 + param.x1 | 0) + param.x2 | 0) + param.x3 | 0) + param.x4 | 0) + v | 0;
}

function f2(param, param$1) {
  return (((((param.x0 + param.x1 | 0) + param.x2 | 0) + param.x3 | 0) + param.x4 | 0) + param$1.a | 0) + param$1.b | 0;
}

function f3(param, param$1) {
  let lhs = param.rank;
  let rhs = param$1.rank;
  if (typeof lhs !== "object") {
    lhs === "Uninitialized";
  } else {
    if (typeof rhs === "object") {
      return Caml.int_compare(lhs._0, rhs._0);
    }
    rhs === "Uninitialized";
  }
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "fun_pattern_match.res",
        33,
        9
      ]
    }
  });
}

function f4(param, param$1) {
  let lhs = param.rank;
  let rhs = param$1.rank;
  if (typeof lhs !== "object") {
    lhs === "Uninitialized";
  } else {
    if (typeof rhs === "object") {
      return Caml.int_compare(lhs._0, rhs._0);
    }
    rhs === "Uninitialized";
  }
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "fun_pattern_match.res",
        39,
        9
      ]
    }
  });
}

let x = {
  NAME: "A",
  VAL: r
};

function r() {
  return x;
}

let match = r();

let v = match.VAL();

console.log(v);

function handle_tuple(x, y) {
  if (x !== 0) {
    if (x === 1 && y === 2) {
      return 3;
    }
    
  } else if (y === 1) {
    return 2;
  }
  console.log([
    x,
    y
  ]);
  return x + y | 0;
}

exports.f = f;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.r = r;
exports.v = v;
exports.handle_tuple = handle_tuple;
/* match Not a pure module */

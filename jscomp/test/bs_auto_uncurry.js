// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Caml_splice_call = require("../../lib/js/caml_splice_call.js");

let Curry = {};

let Block = {};

let xbs = Array.prototype.map.call([
  1,
  2,
  3,
  5
], (function (x) {
  return x + 1 | 0;
}));

function f(cb) {
  return Array.prototype.map.call([
    1,
    2,
    3,
    4
  ], cb);
}

let xs = Array.prototype.map.call([
  [
    1,
    2
  ],
  [
    1,
    2
  ],
  [
    2,
    1
  ]
], (function (param) {
  return (param[1] + param[0] | 0) + 1 | 0;
}));

function f_0() {
  return hi(function () {
    
  });
}

function f_01() {
  return hi(function (x) {
    if (x === undefined) {
      console.log("x");
      return;
    }
    
  });
}

function f_02(xs) {
  return hi(function (x) {
    xs.contents = x;
    console.log("x");
  });
}

function f_03(xs, u) {
  return hi(u);
}

function h(x, y, z) {
  return map2(x, y, z);
}

function h1(x, y, u, z) {
  return map2(x, y, z(u));
}

function add3(x) {
  return function (y, z) {
    return (x + y | 0) + z | 0;
  };
}

function h2(x) {
  return ff(x, (function (prim0, prim1) {
    return prim0 + prim1 | 0;
  }));
}

function h3(x) {
  return ff(x, (function (y, z) {
    return (1 + y | 0) + z | 0;
  }));
}

function h4(x) {
  return ff1(x, 3, (function (y, z) {
    return (1 + y | 0) + z | 0;
  }));
}

function h5(x) {
  return ff2(x, "3", (function (y, z) {
    return (2 + y | 0) + z | 0;
  }));
}

function add(x, y) {
  console.log([
    x,
    y
  ]);
  return x + y | 0;
}

function h6(x) {
  return ff2(x, "3", add);
}

function unit_magic() {
  console.log("noinline");
  console.log("noinline");
  return 3;
}

let f_unit_magic = unit_magic();

function hh(xs, a) {
  Caml_splice_call.spliceApply(f_0002, [
    xs,
    a
  ]);
}

exports.Curry = Curry;
exports.Block = Block;
exports.xbs = xbs;
exports.f = f;
exports.xs = xs;
exports.f_0 = f_0;
exports.f_01 = f_01;
exports.f_02 = f_02;
exports.f_03 = f_03;
exports.h = h;
exports.h1 = h1;
exports.add3 = add3;
exports.h2 = h2;
exports.h3 = h3;
exports.h4 = h4;
exports.h5 = h5;
exports.add = add;
exports.h6 = h6;
exports.unit_magic = unit_magic;
exports.f_unit_magic = f_unit_magic;
exports.hh = hh;
/* xbs Not a pure module */

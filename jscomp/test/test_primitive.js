// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Caml_array = require("../../lib/js/caml_array.js");
let Caml_string = require("../../lib/js/caml_string.js");
let CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");

function a4(prim) {
  return [
    "File \"test_primitive.res\", line 29, characters 9-19",
    prim
  ];
}

function a5(prim) {
  return [
    30,
    prim
  ];
}

function a6(prim) {
  return [
    [
      "test_primitive.res",
      31,
      9,
      19
    ],
    prim
  ];
}

let test_float = 3;

let test_abs = Math.abs(3.0);

let v = [
  1.0,
  2.0
];

let xxx = "a";

let a = Caml_string.get(xxx, 0);

function u(b) {
  if (b) {
    console.log(1);
    return 32;
  } else {
    return 7;
  }
}

function f2(h, b, param) {
  return h(b ? 32 : 7);
}

Caml_array.set(v, 1, 3.0);

let unboxed_x = {
  u: 0,
  v: 0
};

function gg(x) {
  x.u = 0;
}

function f(x) {
  return x.length;
}

let is_lazy_force = CamlinternalLazy.force;

function fib(n) {
  if (n === 0 || n === 1) {
    return 1;
  }
  let fib1 = fib(n - 1 | 0);
  let fib2 = fib(n - 2 | 0);
  return (fib1 + fib2 | 0) + 3 | 0;
}

let a0 = "File \"test_primitive.res\", line 25, characters 9-16";

let a1 = "Test_primitive";

let a2 = 27;

let a3 = "Test_primitive";

let xx = [
  0,
  0
];

exports.a0 = a0;
exports.a1 = a1;
exports.a2 = a2;
exports.a3 = a3;
exports.a4 = a4;
exports.a5 = a5;
exports.a6 = a6;
exports.test_float = test_float;
exports.test_abs = test_abs;
exports.v = v;
exports.xxx = xxx;
exports.a = a;
exports.u = u;
exports.f2 = f2;
exports.xx = xx;
exports.unboxed_x = unboxed_x;
exports.gg = gg;
exports.f = f;
exports.is_lazy_force = is_lazy_force;
exports.fib = fib;
/* test_abs Not a pure module */

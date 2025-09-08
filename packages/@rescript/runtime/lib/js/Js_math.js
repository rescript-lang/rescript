'use strict';

let Js_int = require("./Js_int.js");

function unsafe_ceil(prim) {
  return Math.ceil(prim);
}

function ceil_int(f) {
  if (f > Js_int.max) {
    return Js_int.max;
  } else if (f < Js_int.min) {
    return Js_int.min;
  } else {
    return Math.ceil(f);
  }
}

function unsafe_floor(prim) {
  return Math.floor(prim);
}

function floor_int(f) {
  if (f > Js_int.max) {
    return Js_int.max;
  } else if (f < Js_int.min) {
    return Js_int.min;
  } else {
    return Math.floor(f);
  }
}

function random_int(min, max) {
  return floor_int(Math.random() * (max - min | 0)) + min | 0;
}

let ceil = ceil_int;

let floor = floor_int;

exports.unsafe_ceil = unsafe_ceil;
exports.ceil_int = ceil_int;
exports.ceil = ceil;
exports.unsafe_floor = unsafe_floor;
exports.floor_int = floor_int;
exports.floor = floor;
exports.random_int = random_int;
/* No side effect */

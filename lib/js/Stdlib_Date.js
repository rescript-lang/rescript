'use strict';

let Primitive_float = require("./Primitive_float.js");

let UTC = {};

function equal(a, b) {
  return a.getTime() === b.getTime();
}

function compare(a, b) {
  return Primitive_float.compare(a.getTime(), b.getTime());
}

exports.UTC = UTC;
exports.equal = equal;
exports.compare = compare;
/* No side effect */

'use strict';

let Primitive_option = require("./Primitive_option.js");

function bind(x, f) {
  if (x == null) {
    return x;
  } else {
    return f(x);
  }
}

function iter(x, f) {
  if (!(x == null)) {
    return f(x);
  }
}

function fromOption(x) {
  if (x !== undefined) {
    return Primitive_option.valFromOption(x);
  }
}

let from_opt = fromOption;

exports.bind = bind;
exports.iter = iter;
exports.fromOption = fromOption;
exports.from_opt = from_opt;
/* No side effect */

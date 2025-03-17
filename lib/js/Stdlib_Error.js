'use strict';

let Primitive_option = require("./Primitive_option.js");

function fromException(exn) {
  if (exn.TAG === "Ok") {
    return;
  } else {
    return Primitive_option.some(exn._0);
  }
}

let $$EvalError = {};

let $$RangeError = {};

let $$ReferenceError = {};

let $$SyntaxError = {};

let $$TypeError = {};

let $$URIError = {};

function panic(msg) {
  throw new Error("Panic! " + msg);
}

exports.fromException = fromException;
exports.$$EvalError = $$EvalError;
exports.$$RangeError = $$RangeError;
exports.$$ReferenceError = $$ReferenceError;
exports.$$SyntaxError = $$SyntaxError;
exports.$$TypeError = $$TypeError;
exports.$$URIError = $$URIError;
exports.panic = panic;
/* No side effect */

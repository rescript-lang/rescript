'use strict';

let Stdlib_Exn = require("./Stdlib_Exn.cjs");
let Primitive_option = require("./Primitive_option.cjs");

function fromException(exn) {
  if (exn.RE_EXN_ID === Stdlib_Exn.$$Error) {
    return Primitive_option.some(exn._1);
  }
}

let $$EvalError = {};

let $$RangeError = {};

let $$ReferenceError = {};

let $$SyntaxError = {};

let $$TypeError = {};

let $$URIError = {};

function panic(msg) {
  throw new Error(`Panic! ` + msg);
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

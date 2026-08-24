'use strict';

let Primitive_exceptions = require("./Primitive_exceptions.cjs");

function failwith(s) {
  throw {
    RE_EXN_ID: "Failure",
    _1: s,
    Error: new Error()
  };
}

function invalid_arg(s) {
  throw {
    RE_EXN_ID: "Invalid_argument",
    _1: s,
    Error: new Error()
  };
}

let Exit = /* @__PURE__ */Primitive_exceptions.create("Pervasives.Exit");

exports.failwith = failwith;
exports.invalid_arg = invalid_arg;
exports.Exit = Exit;
/* No side effect */

'use strict';

let Stdlib_Iterator = require("./Stdlib_Iterator.cjs");

let next = Stdlib_Iterator.next;

let nextValue = Stdlib_Iterator.nextValue;

function returnValue(generator, value) {
  return Stdlib_Iterator.normalizeResult(generator.return(value));
}

function throwError(generator, error) {
  return Stdlib_Iterator.normalizeResult(generator.throw(error));
}

exports.next = next;
exports.nextValue = nextValue;
exports.returnValue = returnValue;
exports.throwError = throwError;
/* No side effect */

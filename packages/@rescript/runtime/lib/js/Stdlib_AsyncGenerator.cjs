'use strict';

let Stdlib_AsyncIterator = require("./Stdlib_AsyncIterator.cjs");

let next = Stdlib_AsyncIterator.next;

let nextValue = Stdlib_AsyncIterator.nextValue;

async function returnValue(generator, value) {
  return Stdlib_AsyncIterator.normalizeResult(await generator.return(value));
}

async function throwError(generator, error) {
  return Stdlib_AsyncIterator.normalizeResult(await generator.throw(error));
}

exports.next = next;
exports.nextValue = nextValue;
exports.returnValue = returnValue;
exports.throwError = throwError;
/* No side effect */

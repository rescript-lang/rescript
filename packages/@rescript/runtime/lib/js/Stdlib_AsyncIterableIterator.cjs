'use strict';

let Stdlib_AsyncIterator = require("./Stdlib_AsyncIterator.cjs");

let make = (function makeAsyncIterableIterator(next) {
  return {
    next,
    [Symbol.asyncIterator]() {
      return this;
    }
  }
});

let next = Stdlib_AsyncIterator.next;

let nextValue = Stdlib_AsyncIterator.nextValue;

async function forEach(iterator, f) {
  let iteratorDone = false;
  while (!iteratorDone) {
    let match = await Stdlib_AsyncIterator.next(iterator);
    if (match.done === false) {
      f(match.value);
    } else {
      iteratorDone = true;
    }
  };
}

exports.make = make;
exports.next = next;
exports.nextValue = nextValue;
exports.forEach = forEach;
/* No side effect */

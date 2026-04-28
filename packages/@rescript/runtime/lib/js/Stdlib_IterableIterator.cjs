'use strict';

let Stdlib_Iterator = require("./Stdlib_Iterator.cjs");

let make = (function makeIterableIterator(next) {
  return {
    next,
    [Symbol.iterator]() {
      return this;
    }
  }
});

let next = Stdlib_Iterator.next;

let nextValue = Stdlib_Iterator.nextValue;

exports.make = make;
exports.next = next;
exports.nextValue = nextValue;
/* No side effect */

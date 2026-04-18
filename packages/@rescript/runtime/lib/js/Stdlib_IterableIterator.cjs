'use strict';


let make = (function makeIterableIterator(next) {
  return {
    next,
    [Symbol.iterator]() {
      return this;
    }
  }
});

exports.make = make;
/* No side effect */

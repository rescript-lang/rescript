

import * as Stdlib_Iterator from "./Stdlib_Iterator.mjs";

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

export {
  make,
  next,
  nextValue,
}
/* No side effect */

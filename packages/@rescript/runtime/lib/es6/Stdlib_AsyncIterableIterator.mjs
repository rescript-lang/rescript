

import * as Stdlib_AsyncIterator from "./Stdlib_AsyncIterator.mjs";

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

export {
  make,
  next,
  nextValue,
  forEach,
}
/* No side effect */



import * as Primitive_option from "./Primitive_option.js";

function value(v) {
  return {
    done: false,
    value: Primitive_option.some(v)
  };
}

function done(finalValue) {
  return {
    done: true,
    value: finalValue
  };
}

async function forEach(iterator, f) {
  let iteratorDone = false;
  while (!iteratorDone) {
    let match = await iterator.next();
    f(match.value);
    iteratorDone = match.done;
  };
}

let make = (function makeAsyncIterator(next) {
  return {
    next,
    [Symbol.asyncIterator]() {
      return this;
    }
  }
});

export {
  make,
  value,
  done,
  forEach,
}
/* No side effect */

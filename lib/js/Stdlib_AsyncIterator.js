'use strict';


function value(v) {
  return {
    done: false,
    value: v
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

exports.make = make;
exports.value = value;
exports.done = done;
exports.forEach = forEach;
/* No side effect */

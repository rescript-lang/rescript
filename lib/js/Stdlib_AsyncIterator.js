'use strict';

let Primitive_option = require("./Primitive_option.js");

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

exports.make = make;
exports.value = value;
exports.done = done;
exports.forEach = forEach;
/* No side effect */

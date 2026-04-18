'use strict';


let value = (value => ({done: false, value}));

let done = (() => ({done: true, value: undefined}));

let doneWithValue = (value => ({done: true, value}));

let make = (function makeIterator(next) {
  return {
    next
  }
});

exports.value = value;
exports.done = done;
exports.doneWithValue = doneWithValue;
exports.make = make;
/* No side effect */

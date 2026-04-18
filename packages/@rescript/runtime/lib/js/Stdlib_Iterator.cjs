'use strict';


let normalizeResult = (result => result.done ? {done: true, value: result.value} : {done: false, value: result.value});

let value = (value => ({done: false, value}));

let done = (() => ({done: true, value: undefined}));

let doneWithValue = (value => ({done: true, value}));

function next(iterator) {
  return normalizeResult(iterator.next());
}

function nextValue(iterator, value) {
  return normalizeResult(iterator.next(value));
}

let make = (function makeIterator(next) {
  return {
    next
  }
});

function nextRaw(prim) {
  return prim.next();
}

function nextValueRaw(prim0, prim1) {
  return prim0.next(prim1);
}

exports.normalizeResult = normalizeResult;
exports.value = value;
exports.done = done;
exports.doneWithValue = doneWithValue;
exports.nextRaw = nextRaw;
exports.next = next;
exports.nextValueRaw = nextValueRaw;
exports.nextValue = nextValue;
exports.make = make;
/* No side effect */

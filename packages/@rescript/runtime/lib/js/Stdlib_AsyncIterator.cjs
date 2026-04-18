'use strict';


let value = (value => ({done: false, value}));

let done = (() => ({done: true, value: undefined}));

let doneWithValue = (value => ({done: true, value}));

let normalizeResult = (result => result.done ? {done: true, value: result.value} : {done: false, value: result.value});

async function next(iterator) {
  return normalizeResult(await iterator.next());
}

async function nextValue(iterator, value) {
  return normalizeResult(await iterator.next(value));
}

async function forEach(iterator, f) {
  let iteratorDone = false;
  while (!iteratorDone) {
    let match = await next(iterator);
    if (match.done === false) {
      f(match.value);
    } else {
      iteratorDone = true;
    }
  };
}

let make = (function makeAsyncIterator(next) {
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
exports.forEach = forEach;
exports.make = make;
/* No side effect */

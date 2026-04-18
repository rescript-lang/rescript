


let value = (value => ({done: false, value}));

let done = (() => ({done: true, value: undefined}));

let doneWithValue = (value => ({done: true, value}));

let make = (function makeIterator(next) {
  return {
    next
  }
});

export {
  value,
  done,
  doneWithValue,
  make,
}
/* No side effect */




let value = (value => ({done: false, value}));

let done = (() => ({done: true, value: undefined}));

let doneWithValue = (value => ({done: true, value}));

async function forEach(iterator, f) {
  let iteratorDone = false;
  while (!iteratorDone) {
    let match = await iterator.next();
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

export {
  value,
  done,
  doneWithValue,
  forEach,
  make,
}
/* No side effect */

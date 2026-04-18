


let make = (function makeAsyncIterableIterator(next) {
  return {
    next,
    [Symbol.asyncIterator]() {
      return this;
    }
  }
});

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

export {
  make,
  forEach,
}
/* No side effect */




let make = (function makeIterableIterator(next) {
  return {
    next,
    [Symbol.iterator]() {
      return this;
    }
  }
});

export {
  make,
}
/* No side effect */

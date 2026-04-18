

import * as Stdlib_Iterator from "./Stdlib_Iterator.mjs";

let next = Stdlib_Iterator.next;

let nextValue = Stdlib_Iterator.nextValue;

function returnValue(generator, value) {
  return Stdlib_Iterator.normalizeResult(generator.return(value));
}

function throwError(generator, error) {
  return Stdlib_Iterator.normalizeResult(generator.throw(error));
}

export {
  next,
  nextValue,
  returnValue,
  throwError,
}
/* No side effect */

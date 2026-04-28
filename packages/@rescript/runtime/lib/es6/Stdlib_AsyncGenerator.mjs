

import * as Stdlib_AsyncIterator from "./Stdlib_AsyncIterator.mjs";

let next = Stdlib_AsyncIterator.next;

let nextValue = Stdlib_AsyncIterator.nextValue;

async function returnValue(generator, value) {
  return Stdlib_AsyncIterator.normalizeResult(await generator.return(value));
}

async function throwError(generator, error) {
  return Stdlib_AsyncIterator.normalizeResult(await generator.throw(error));
}

export {
  next,
  nextValue,
  returnValue,
  throwError,
}
/* No side effect */

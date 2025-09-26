

import * as Primitive_exceptions from "./Primitive_exceptions.js";

function $$catch(promise, callback) {
  return promise.catch(err => callback(Primitive_exceptions.internalToException(err)));
}

function sleep(ms) {
  return new Promise((resolve, param) => {
    setTimeout(resolve, ms);
  });
}

export {
  $$catch,
  sleep,
}
/* No side effect */

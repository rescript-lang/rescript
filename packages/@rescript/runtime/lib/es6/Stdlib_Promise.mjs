

import * as Primitive_exceptions from "./Primitive_exceptions.mjs";

function $$catch(promise, callback) {
  return promise.catch(err => callback(Primitive_exceptions.internalToException(err)));
}

export {
  $$catch,
}
/* No side effect */

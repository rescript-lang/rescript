

import * as Primitive_float from "./Primitive_float.js";

let UTC = {};

function equal(a, b) {
  return a.getTime() === b.getTime();
}

function compare(a, b) {
  return Primitive_float.compare(a.getTime(), b.getTime());
}

export {
  UTC,
  equal,
  compare,
}
/* No side effect */

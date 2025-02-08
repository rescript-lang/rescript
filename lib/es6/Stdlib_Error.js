

import * as Primitive_option from "./Primitive_option.js";

function fromException(exn) {
  if (exn.TAG === "Ok") {
    return;
  } else {
    return Primitive_option.some(exn._0);
  }
}

let $$EvalError = {};

let $$RangeError = {};

let $$ReferenceError = {};

let $$SyntaxError = {};

let $$TypeError = {};

let $$URIError = {};

function panic(msg) {
  throw new Error("Panic! " + msg);
}

export {
  fromException,
  $$EvalError,
  $$RangeError,
  $$ReferenceError,
  $$SyntaxError,
  $$TypeError,
  $$URIError,
  panic,
}
/* No side effect */

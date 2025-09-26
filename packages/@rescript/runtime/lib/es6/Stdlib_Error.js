

import * as Stdlib_Exn from "./Stdlib_Exn.js";
import * as Primitive_option from "./Primitive_option.js";

function fromException(exn) {
  if (exn.RE_EXN_ID === Stdlib_Exn.$$Error) {
    return Primitive_option.some(exn._1);
  }
}

let $$EvalError = {};

let $$RangeError = {};

let $$ReferenceError = {};

let $$SyntaxError = {};

let $$TypeError = {};

let $$URIError = {};

function panic(msg) {
  throw new Error(`Panic! ` + msg);
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

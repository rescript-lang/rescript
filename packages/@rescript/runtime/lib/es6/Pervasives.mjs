

import * as Primitive_exceptions from "./Primitive_exceptions.mjs";

function failwith(s) {
  throw {
    RE_EXN_ID: "Failure",
    _1: s,
    Error: new Error()
  };
}

function invalid_arg(s) {
  throw {
    RE_EXN_ID: "Invalid_argument",
    _1: s,
    Error: new Error()
  };
}

let Exit = /* @__PURE__ */Primitive_exceptions.create("Pervasives.Exit");

export {
  failwith,
  invalid_arg,
  Exit,
}
/* No side effect */

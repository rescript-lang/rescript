'use strict';

let Stdlib_JsExn = require("./Stdlib_JsExn.cjs");
let Primitive_exceptions = require("./Primitive_exceptions.cjs");

function fromString(value) {
  try {
    return Temporal.Duration.from(value);
  } catch (raw_error) {
    let error = Primitive_exceptions.internalToException(raw_error);
    if (error.RE_EXN_ID === "JsExn") {
      let error$1 = error._1;
      let match = Stdlib_JsExn.name(error$1);
      if (match === "RangeError") {
        return;
      }
      throw error$1;
    }
    throw error;
  }
}

exports.fromString = fromString;
/* Stdlib_JsExn Not a pure module */

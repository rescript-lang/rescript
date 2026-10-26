'use strict';

let Primitive_exceptions = require("./Primitive_exceptions.cjs");

function isEvaluated(l) {
  return l.LAZY_DONE;
}

let Undefined = /* @__PURE__ */Primitive_exceptions.create("Stdlib_Lazy.Undefined");

function raise_undefined() {
  throw {
    RE_EXN_ID: Undefined,
    Error: new Error()
  };
}

function get(lzv) {
  if (lzv.LAZY_DONE) {
    return lzv.VAL;
  } else {
    let closure = lzv.VAL;
    lzv.VAL = raise_undefined;
    try {
      let result = closure();
      lzv.VAL = result;
      lzv.LAZY_DONE = true;
      return result;
    } catch (e) {
      lzv.VAL = () => {
        throw e;
      };
      throw e;
    }
  }
}

function make(closure) {
  return {
    LAZY_DONE: false,
    VAL: closure
  };
}

exports.make = make;
exports.get = get;
exports.isEvaluated = isEvaluated;
exports.Undefined = Undefined;
/* No side effect */



import * as Primitive_exceptions from "./Primitive_exceptions.mjs";

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

export {
  make,
  get,
  isEvaluated,
  Undefined,
}
/* No side effect */

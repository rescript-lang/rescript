

import * as Primitive_option from "./Primitive_option.js";

function test(x) {
  return x === null;
}

function getExn(f) {
  if (f !== null) {
    return f;
  }
  throw new Error("Js.Null.getExn");
}

function bind(x, f) {
  if (x !== null) {
    return f(x);
  } else {
    return null;
  }
}

function iter(x, f) {
  if (x !== null) {
    return f(x);
  }
}

function fromOption(x) {
  if (x !== undefined) {
    return Primitive_option.valFromOption(x);
  } else {
    return null;
  }
}

let from_opt = fromOption;

export {
  test,
  getExn,
  bind,
  iter,
  fromOption,
  from_opt,
}
/* No side effect */

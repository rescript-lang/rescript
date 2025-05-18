

import * as Primitive_option from "./Primitive_option.js";

function test(x) {
  return x === undefined;
}

function testAny(x) {
  return x === undefined;
}

function getExn(f) {
  if (f !== undefined) {
    return f;
  }
  throw new Error("Js.Undefined.getExn");
}

function bind(x, f) {
  if (x !== undefined) {
    return f(x);
  }
  
}

function iter(x, f) {
  if (x !== undefined) {
    return f(x);
  }
  
}

function fromOption(x) {
  if (x !== undefined) {
    return Primitive_option.valFromOption(x);
  }
  
}

let from_opt = fromOption;

export {
  test,
  testAny,
  getExn,
  bind,
  iter,
  fromOption,
  from_opt,
}
/* No side effect */

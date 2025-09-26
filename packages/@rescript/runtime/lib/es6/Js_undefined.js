

import * as Primitive_option from "./Primitive_option.js";

function test(x) {
  return x === undefined;
}

function testAny(x) {
  return x === undefined;
}

function getExn(f) {
  let x = Primitive_option.fromUndefined(f);
  if (x !== undefined) {
    return Primitive_option.valFromOption(x);
  }
  throw new Error("Js.Undefined.getExn");
}

function bind(x, f) {
  let x$1 = Primitive_option.fromUndefined(x);
  if (x$1 !== undefined) {
    return f(Primitive_option.valFromOption(x$1));
  }
}

function iter(x, f) {
  let x$1 = Primitive_option.fromUndefined(x);
  if (x$1 !== undefined) {
    return f(Primitive_option.valFromOption(x$1));
  }
}

function fromOption(x) {
  if (x !== undefined) {
    return Primitive_option.valFromOption(x);
  }
}

let from_opt = fromOption;

let toOption = Primitive_option.fromUndefined;

let to_opt = Primitive_option.fromUndefined;

export {
  test,
  testAny,
  getExn,
  bind,
  iter,
  fromOption,
  from_opt,
  toOption,
  to_opt,
}
/* No side effect */

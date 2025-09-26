

import * as Primitive_option from "./Primitive_option.js";

function some(x) {
  return Primitive_option.some(x);
}

function isSome(x) {
  return x !== undefined;
}

function isSomeValue(eq, v, x) {
  if (x !== undefined) {
    return eq(v, Primitive_option.valFromOption(x));
  } else {
    return false;
  }
}

function isNone(x) {
  return x === undefined;
}

function getExn(x) {
  if (x !== undefined) {
    return Primitive_option.valFromOption(x);
  }
  throw new Error("getExn");
}

function equal(eq, a, b) {
  if (a !== undefined) {
    if (b !== undefined) {
      return eq(Primitive_option.valFromOption(a), Primitive_option.valFromOption(b));
    } else {
      return false;
    }
  } else {
    return b === undefined;
  }
}

function andThen(f, x) {
  if (x !== undefined) {
    return f(Primitive_option.valFromOption(x));
  }
}

function map(f, x) {
  if (x !== undefined) {
    return Primitive_option.some(f(Primitive_option.valFromOption(x)));
  }
}

function getWithDefault(a, x) {
  if (x !== undefined) {
    return Primitive_option.valFromOption(x);
  } else {
    return a;
  }
}

function filter(f, x) {
  if (x === undefined) {
    return;
  }
  let x$1 = Primitive_option.valFromOption(x);
  if (f(x$1)) {
    return Primitive_option.some(x$1);
  }
}

function firstSome(a, b) {
  if (a !== undefined) {
    return a;
  } else if (b !== undefined) {
    return b;
  } else {
    return;
  }
}

let $$default = getWithDefault;

export {
  some,
  isSome,
  isSomeValue,
  isNone,
  getExn,
  equal,
  andThen,
  map,
  getWithDefault,
  $$default as default,
  filter,
  firstSome,
}
/* No side effect */

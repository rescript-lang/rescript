

import * as Stdlib_Option from "./Stdlib_Option.js";
import * as Primitive_option from "./Primitive_option.js";

function fromOption(option) {
  if (option !== undefined) {
    return Primitive_option.valFromOption(option);
  } else {
    return null;
  }
}

function equal(a, b, eq) {
  return Stdlib_Option.equal(a === null ? undefined : Primitive_option.some(a), b === null ? undefined : Primitive_option.some(b), eq);
}

function compare(a, b, cmp) {
  return Stdlib_Option.compare(a === null ? undefined : Primitive_option.some(a), b === null ? undefined : Primitive_option.some(b), cmp);
}

function getOr(value, $$default) {
  if (value !== null) {
    return value;
  } else {
    return $$default;
  }
}

function getExn(value) {
  if (value !== null) {
    return value;
  }
  throw {
    RE_EXN_ID: "Invalid_argument",
    _1: "Null.getExn: value is null",
    Error: new Error()
  };
}

function forEach(value, f) {
  if (value !== null) {
    return f(value);
  }
  
}

function map(value, f) {
  if (value !== null) {
    return f(value);
  } else {
    return null;
  }
}

function mapOr(value, $$default, f) {
  if (value !== null) {
    return f(value);
  } else {
    return $$default;
  }
}

function flatMap(value, f) {
  if (value !== null) {
    return f(value);
  } else {
    return null;
  }
}

let getWithDefault = getOr;

let mapWithDefault = mapOr;

export {
  equal,
  compare,
  fromOption,
  getOr,
  getWithDefault,
  getExn,
  forEach,
  map,
  mapOr,
  mapWithDefault,
  flatMap,
}
/* No side effect */

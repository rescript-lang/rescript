

import * as Stdlib_Option from "./Stdlib_Option.js";

function fromOption(option) {
  if (option !== undefined) {
    return option;
  }
  
}

function equal(a, b, eq) {
  return Stdlib_Option.equal((a == null) ? undefined : a, (b == null) ? undefined : b, eq);
}

function compare(a, b, cmp) {
  return Stdlib_Option.compare((a == null) ? undefined : a, (b == null) ? undefined : b, cmp);
}

function getOr(value, $$default) {
  if (value == null) {
    return $$default;
  } else {
    return value;
  }
}

function getExn(value) {
  if (!(value == null)) {
    return value;
  }
  throw {
    RE_EXN_ID: "Invalid_argument",
    _1: "Nullable.getExn: value is null or undefined",
    Error: new Error()
  };
}

function forEach(value, f) {
  if (!(value == null)) {
    return f(value);
  }
  
}

function map(value, f) {
  if (value == null) {
    return value;
  } else {
    return f(value);
  }
}

function mapOr(value, $$default, f) {
  if (value == null) {
    return $$default;
  } else {
    return f(value);
  }
}

function flatMap(value, f) {
  if (value == null) {
    return value;
  } else {
    return f(value);
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

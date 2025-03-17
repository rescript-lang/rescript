'use strict';

let Stdlib_Option = require("./Stdlib_Option.js");
let Primitive_option = require("./Primitive_option.js");

function fromOption(option) {
  if (option !== undefined) {
    return Primitive_option.valFromOption(option);
  }
  
}

function equal(a, b, eq) {
  return Stdlib_Option.equal((a == null) ? undefined : Primitive_option.some(a), (b == null) ? undefined : Primitive_option.some(b), eq);
}

function compare(a, b, cmp) {
  return Stdlib_Option.compare((a == null) ? undefined : Primitive_option.some(a), (b == null) ? undefined : Primitive_option.some(b), cmp);
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

exports.equal = equal;
exports.compare = compare;
exports.fromOption = fromOption;
exports.getOr = getOr;
exports.getWithDefault = getWithDefault;
exports.getExn = getExn;
exports.forEach = forEach;
exports.map = map;
exports.mapOr = mapOr;
exports.mapWithDefault = mapWithDefault;
exports.flatMap = flatMap;
/* No side effect */

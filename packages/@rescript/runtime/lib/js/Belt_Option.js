'use strict';

let Primitive_option = require("./Primitive_option.js");

function keep(opt, p) {
  if (opt !== undefined && p(Primitive_option.valFromOption(opt))) {
    return opt;
  }
}

function forEach(opt, f) {
  if (opt !== undefined) {
    return f(Primitive_option.valFromOption(opt));
  }
}

function getOrThrow(x) {
  if (x !== undefined) {
    return Primitive_option.valFromOption(x);
  }
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}

function mapWithDefault(opt, $$default, f) {
  if (opt !== undefined) {
    return f(Primitive_option.valFromOption(opt));
  } else {
    return $$default;
  }
}

function map(opt, f) {
  if (opt !== undefined) {
    return Primitive_option.some(f(Primitive_option.valFromOption(opt)));
  }
}

function flatMap(opt, f) {
  if (opt !== undefined) {
    return f(Primitive_option.valFromOption(opt));
  }
}

function getWithDefault(opt, $$default) {
  if (opt !== undefined) {
    return Primitive_option.valFromOption(opt);
  } else {
    return $$default;
  }
}

function orElse(opt, other) {
  if (opt !== undefined) {
    return opt;
  } else {
    return other;
  }
}

function isSome(x) {
  return x !== undefined;
}

function isNone(x) {
  return x === undefined;
}

function eq(a, b, f) {
  if (a !== undefined) {
    if (b !== undefined) {
      return f(Primitive_option.valFromOption(a), Primitive_option.valFromOption(b));
    } else {
      return false;
    }
  } else {
    return b === undefined;
  }
}

function cmp(a, b, f) {
  if (a !== undefined) {
    if (b !== undefined) {
      return f(Primitive_option.valFromOption(a), Primitive_option.valFromOption(b));
    } else {
      return 1;
    }
  } else if (b !== undefined) {
    return -1;
  } else {
    return 0;
  }
}

let keepU = keep;

let forEachU = forEach;

let getExn = getOrThrow;

let mapWithDefaultU = mapWithDefault;

let mapU = map;

let flatMapU = flatMap;

let eqU = eq;

let cmpU = cmp;

exports.keepU = keepU;
exports.keep = keep;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.getExn = getExn;
exports.getOrThrow = getOrThrow;
exports.mapWithDefaultU = mapWithDefaultU;
exports.mapWithDefault = mapWithDefault;
exports.mapU = mapU;
exports.map = map;
exports.flatMapU = flatMapU;
exports.flatMap = flatMap;
exports.getWithDefault = getWithDefault;
exports.orElse = orElse;
exports.isSome = isSome;
exports.isNone = isNone;
exports.eqU = eqU;
exports.eq = eq;
exports.cmpU = cmpU;
exports.cmp = cmp;
/* No side effect */

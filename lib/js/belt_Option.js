'use strict';

let Caml_option = require("./caml_option.js");
let Caml_js_exceptions = require("./caml_js_exceptions.js");

function keep(opt, p) {
  if (opt !== undefined && p(Caml_option.valFromOption(opt))) {
    return opt;
  }
  
}

function forEach(opt, f) {
  if (opt !== undefined) {
    return f(Caml_option.valFromOption(opt));
  }
  
}

function getExn(x) {
  if (x !== undefined) {
    return Caml_option.valFromOption(x);
  }
  throw Caml_js_exceptions.internalMakeExn("Not_found");
}

function mapWithDefault(opt, $$default, f) {
  if (opt !== undefined) {
    return f(Caml_option.valFromOption(opt));
  } else {
    return $$default;
  }
}

function map(opt, f) {
  if (opt !== undefined) {
    return Caml_option.some(f(Caml_option.valFromOption(opt)));
  }
  
}

function flatMap(opt, f) {
  if (opt !== undefined) {
    return f(Caml_option.valFromOption(opt));
  }
  
}

function getWithDefault(opt, $$default) {
  if (opt !== undefined) {
    return Caml_option.valFromOption(opt);
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
      return f(Caml_option.valFromOption(a), Caml_option.valFromOption(b));
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
      return f(Caml_option.valFromOption(a), Caml_option.valFromOption(b));
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



import * as Primitive_exceptions from "./Primitive_exceptions.js";

function failwith(s) {
  throw {
    RE_EXN_ID: "Failure",
    _1: s,
    Error: new Error()
  };
}

function invalid_arg(s) {
  throw {
    RE_EXN_ID: "Invalid_argument",
    _1: s,
    Error: new Error()
  };
}

let Exit = /* @__PURE__ */Primitive_exceptions.create("Pervasives.Exit");

function abs(x) {
  if (x >= 0) {
    return x;
  } else {
    return -x | 0;
  }
}

function lnot(x) {
  return x ^ -1;
}

let min_int = -2147483648;

function classify_float(x) {
  if (isFinite(x)) {
    if (Math.abs(x) >= 2.22507385850720138e-308) {
      return "FP_normal";
    } else if (x !== 0) {
      return "FP_subnormal";
    } else {
      return "FP_zero";
    }
  } else if (isNaN(x)) {
    return "FP_nan";
  } else {
    return "FP_infinite";
  }
}

function char_of_int(n) {
  if (n < 0 || n > 255) {
    throw {
      RE_EXN_ID: "Invalid_argument",
      _1: "char_of_int",
      Error: new Error()
    };
  }
  return n;
}

function string_of_bool(b) {
  if (b) {
    return "true";
  } else {
    return "false";
  }
}

function bool_of_string(param) {
  switch (param) {
    case "false" :
      return false;
    case "true" :
      return true;
    default:
      throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "bool_of_string",
        Error: new Error()
      };
  }
}

function bool_of_string_opt(param) {
  switch (param) {
    case "false" :
      return false;
    case "true" :
      return true;
    default:
      return;
  }
}

function int_of_string_opt(s) {
  let n = Number.parseInt(s);
  if (n === NaN) {
    return;
  } else {
    return n;
  }
}

function $at(l1, l2) {
  if (l1 !== 0) {
    return {
      hd: l1.hd,
      tl: $at(l1.tl, l2)
    };
  } else {
    return l2;
  }
}

let max_int = 2147483647;

let infinity = Infinity;

let neg_infinity = -Infinity;

let max_float = 1.79769313486231571e+308;

let min_float = 2.22507385850720138e-308;

let epsilon_float = 2.22044604925031308e-16;

export {
  failwith,
  invalid_arg,
  Exit,
  abs,
  lnot,
  max_int,
  min_int,
  infinity,
  neg_infinity,
  max_float,
  min_float,
  epsilon_float,
  classify_float,
  char_of_int,
  string_of_bool,
  bool_of_string,
  bool_of_string_opt,
  int_of_string_opt,
  $at,
}
/* No side effect */

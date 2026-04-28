'use strict';


function toString(b) {
  if (b) {
    return "true";
  } else {
    return "false";
  }
}

function fromString(s) {
  switch (s) {
    case "false" :
      return false;
    case "true" :
      return true;
    default:
      return;
  }
}

function fromStringOrThrow(param) {
  switch (param) {
    case "false" :
      return false;
    case "true" :
      return true;
    default:
      throw {
        RE_EXN_ID: "Invalid_argument",
        _1: `Bool.fromStringOrThrow: value is neither "true" nor "false"`,
        Error: new Error()
      };
  }
}

let fromStringExn = fromStringOrThrow;

exports.toString = toString;
exports.fromString = fromString;
exports.fromStringOrThrow = fromStringOrThrow;
exports.fromStringExn = fromStringExn;
/* No side effect */

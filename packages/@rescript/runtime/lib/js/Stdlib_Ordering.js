'use strict';


function isLess(ord) {
  return ord < 0;
}

function isEqual(ord) {
  return ord === 0;
}

function isGreater(ord) {
  return ord > 0;
}

function invert(ord) {
  return - ord;
}

function fromInt(n) {
  if (n < 0) {
    return -1;
  } else if (n > 0) {
    return 1;
  } else {
    return 0;
  }
}

exports.isLess = isLess;
exports.isEqual = isEqual;
exports.isGreater = isGreater;
exports.invert = invert;
exports.fromInt = fromInt;
/* No side effect */

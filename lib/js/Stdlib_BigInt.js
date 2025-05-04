'use strict';


function fromFloat(value) {
  try {
    return BigInt(value);
  } catch (exn) {
    return;
  }
}

function toInt(t) {
  return Number(t) | 0;
}

function bitwiseNot(x) {
  return x ^ -1n;
}

exports.fromFloat = fromFloat;
exports.toInt = toInt;
exports.bitwiseNot = bitwiseNot;
/* No side effect */

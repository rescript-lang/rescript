'use strict';


function toInt(t) {
  return Number(t) | 0;
}

function bitwiseNot(x) {
  return x ^ -1n;
}

exports.toInt = toInt;
exports.bitwiseNot = bitwiseNot;
/* No side effect */

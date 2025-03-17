'use strict';


function toInt(t) {
  return Number(t) | 0;
}

function lnot(x) {
  return x ^ -1n;
}

exports.toInt = toInt;
exports.lnot = lnot;
/* No side effect */

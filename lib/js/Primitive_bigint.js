'use strict';


function compare(x, y) {
  if (x < y) {
    return -1;
  } else if (x === y) {
    return 0;
  } else {
    return 1;
  }
}

function min(x, y) {
  if (x < y) {
    return x;
  } else {
    return y;
  }
}

function max(x, y) {
  if (x > y) {
    return x;
  } else {
    return y;
  }
}

exports.compare = compare;
exports.min = min;
exports.max = max;
/* No side effect */

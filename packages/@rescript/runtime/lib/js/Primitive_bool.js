'use strict';


function compare(x, y) {
  if (x) {
    if (y) {
      return 0;
    } else {
      return 1;
    }
  } else if (y) {
    return -1;
  } else {
    return 0;
  }
}

function min(x, y) {
  if (x) {
    return y;
  } else {
    return x;
  }
}

function max(x, y) {
  if (x) {
    return x;
  } else {
    return y;
  }
}

exports.compare = compare;
exports.min = min;
exports.max = max;
/* No side effect */

'use strict';


let Constants = {};

let fromString = (str => {
  if (!str || !str.trim()) return;
  let num = +str;
  return isNaN(num) ? undefined : num;
});

function clamp(min, max, value) {
  let value$1 = max !== undefined && max < value ? max : value;
  if (min !== undefined && min > value$1) {
    return min;
  } else {
    return value$1;
  }
}

exports.Constants = Constants;
exports.fromString = fromString;
exports.clamp = clamp;
/* No side effect */

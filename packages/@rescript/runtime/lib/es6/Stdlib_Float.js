


let Constants = {};

function fromString(i) {
  let i$1 = parseFloat(i);
  if (isNaN(i$1)) {
    return;
  } else {
    return i$1;
  }
}

function clamp(min, max, value) {
  let value$1 = max !== undefined && max < value ? max : value;
  if (min !== undefined && min > value$1) {
    return min;
  } else {
    return value$1;
  }
}

export {
  Constants,
  fromString,
  clamp,
}
/* No side effect */

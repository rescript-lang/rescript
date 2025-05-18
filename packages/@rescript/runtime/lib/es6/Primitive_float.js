


function compare(x, y) {
  if (x === y) {
    return 0;
  } else if (x < y) {
    return -1;
  } else if (x > y || x === x) {
    return 1;
  } else if (y === y) {
    return -1;
  } else {
    return 0;
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

export {
  compare,
  min,
  max,
}
/* No side effect */

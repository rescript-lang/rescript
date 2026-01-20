


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

export {
  compare,
  min,
  max,
}
/* No side effect */

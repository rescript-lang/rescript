


function toInt(t) {
  return Number(t) | 0;
}

function lnot(x) {
  return x ^ -1n;
}

export {
  toInt,
  lnot,
}
/* No side effect */

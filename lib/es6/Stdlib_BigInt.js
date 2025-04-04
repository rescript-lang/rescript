


function toInt(t) {
  return Number(t) | 0;
}

function bitwiseNot(x) {
  return x ^ -1n;
}

export {
  toInt,
  bitwiseNot,
}
/* No side effect */

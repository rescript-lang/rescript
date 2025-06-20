


function fromString(value) {
  try {
    return BigInt(value);
  } catch (exn) {
    return;
  }
}

function fromFloat(value) {
  try {
    return BigInt(value);
  } catch (exn) {
    return;
  }
}

function toInt(t) {
  return Number(t) | 0;
}

export {
  fromString,
  fromFloat,
  toInt,
}
/* No side effect */

'use strict';


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

exports.fromString = fromString;
exports.fromFloat = fromFloat;
exports.toInt = toInt;
/* No side effect */

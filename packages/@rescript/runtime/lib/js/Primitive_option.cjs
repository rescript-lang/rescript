'use strict';


function isNested(x) {
  return x.BS_PRIVATE_NESTED_SOME_NONE !== undefined;
}

let markers = [];

function marker(depth) {
  let m = markers[depth];
  if (m !== undefined) {
    return m;
  }
  let m$1 = Object.freeze({
    BS_PRIVATE_NESTED_SOME_NONE: depth
  });
  markers[depth] = m$1;
  return m$1;
}

function some(x) {
  if (x === undefined) {
    return marker(0);
  } else if (x !== null && x.BS_PRIVATE_NESTED_SOME_NONE !== undefined) {
    return marker(x.BS_PRIVATE_NESTED_SOME_NONE + 1 | 0);
  } else {
    return x;
  }
}

function fromNullable(x) {
  if (x == null) {
    return;
  } else {
    return some(x);
  }
}

function fromNull(x) {
  if (x === null) {
    return;
  } else {
    return some(x);
  }
}

function valFromOption(x) {
  if (x === null || x.BS_PRIVATE_NESTED_SOME_NONE === undefined) {
    return x;
  }
  let depth = x.BS_PRIVATE_NESTED_SOME_NONE;
  if (depth === 0) {
    return;
  } else {
    return marker(depth - 1 | 0);
  }
}

function unwrapPolyVar(x) {
  if (x !== undefined) {
    return x.VAL;
  } else {
    return x;
  }
}

exports.fromNullable = fromNullable;
exports.fromNull = fromNull;
exports.valFromOption = valFromOption;
exports.some = some;
exports.isNested = isNested;
exports.unwrapPolyVar = unwrapPolyVar;
/* No side effect */

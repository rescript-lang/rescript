'use strict';


function isNested(x) {
  return x.BS_PRIVATE_NESTED_SOME_NONE !== undefined;
}

function some(x) {
  if (x === undefined) {
    return {
      BS_PRIVATE_NESTED_SOME_NONE: 0
    };
  } else if (x !== null && x.BS_PRIVATE_NESTED_SOME_NONE !== undefined) {
    return {
      BS_PRIVATE_NESTED_SOME_NONE: x.BS_PRIVATE_NESTED_SOME_NONE + 1 | 0
    };
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

function fromUndefined(x) {
  if (x === undefined) {
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
    return {
      BS_PRIVATE_NESTED_SOME_NONE: depth - 1 | 0
    };
  }
}

function toUndefined(x) {
  if (x === undefined) {
    return;
  } else {
    return valFromOption(x);
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
exports.fromUndefined = fromUndefined;
exports.fromNull = fromNull;
exports.valFromOption = valFromOption;
exports.some = some;
exports.isNested = isNested;
exports.toUndefined = toUndefined;
exports.unwrapPolyVar = unwrapPolyVar;
/* No side effect */

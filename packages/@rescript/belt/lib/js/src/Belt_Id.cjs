'use strict';


function MakeComparable(M) {
  return M;
}

function comparable(cmp) {
  return {
    cmp: cmp
  };
}

function MakeHashable(M) {
  return M;
}

function hashable(hash, eq) {
  return {
    hash: hash,
    eq: eq
  };
}

let MakeComparableU = MakeComparable;

let comparableU = comparable;

let MakeHashableU = MakeHashable;

let hashableU = hashable;

exports.MakeComparableU = MakeComparableU;
exports.MakeComparable = MakeComparable;
exports.comparableU = comparableU;
exports.comparable = comparable;
exports.MakeHashableU = MakeHashableU;
exports.MakeHashable = MakeHashable;
exports.hashableU = hashableU;
exports.hashable = hashable;
/* No side effect */

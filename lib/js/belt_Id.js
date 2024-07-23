'use strict';

let Curry = require("./curry.js");

function MakeComparableU(M) {
  return M;
}

function MakeComparable(M) {
  let cmp = M.cmp;
  let cmp$1 = Curry.__2(cmp);
  return {
    cmp: cmp$1
  };
}

function comparableU(cmp) {
  return {
    cmp: cmp
  };
}

function comparable(cmp) {
  let cmp$1 = Curry.__2(cmp);
  return {
    cmp: cmp$1
  };
}

function MakeHashableU(M) {
  return M;
}

function MakeHashable(M) {
  let hash = M.hash;
  let hash$1 = Curry.__1(hash);
  let eq = M.eq;
  let eq$1 = Curry.__2(eq);
  return {
    hash: hash$1,
    eq: eq$1
  };
}

function hashableU(hash, eq) {
  return {
    hash: hash,
    eq: eq
  };
}

function hashable(hash, eq) {
  let hash$1 = Curry.__1(hash);
  let eq$1 = Curry.__2(eq);
  return {
    hash: hash$1,
    eq: eq$1
  };
}

exports.MakeComparableU = MakeComparableU;
exports.MakeComparable = MakeComparable;
exports.comparableU = comparableU;
exports.comparable = comparable;
exports.MakeHashableU = MakeHashableU;
exports.MakeHashable = MakeHashable;
exports.hashableU = hashableU;
exports.hashable = hashable;
/* No side effect */

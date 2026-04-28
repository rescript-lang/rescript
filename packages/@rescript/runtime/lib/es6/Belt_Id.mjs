


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

export {
  MakeComparableU,
  MakeComparable,
  comparableU,
  comparable,
  MakeHashableU,
  MakeHashable,
  hashableU,
  hashable,
}
/* No side effect */

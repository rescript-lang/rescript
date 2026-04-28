


function power_2_above(_x, n) {
  while (true) {
    let x = _x;
    if (x >= n) {
      return x;
    }
    if ((x << 1) < x) {
      return x;
    }
    _x = (x << 1);
    continue;
  };
}

function make(hash, eq, hintSize) {
  let s = power_2_above(16, hintSize);
  return {
    size: 0,
    buckets: new Array(s),
    hash: hash,
    eq: eq
  };
}

function clear(h) {
  h.size = 0;
  let h_buckets = h.buckets;
  let len = h_buckets.length;
  for (let i = 0; i < len; ++i) {
    h_buckets[i] = undefined;
  }
}

function isEmpty(h) {
  return h.size === 0;
}

let emptyOpt;

export {
  emptyOpt,
  make,
  clear,
  isEmpty,
}
/* No side effect */

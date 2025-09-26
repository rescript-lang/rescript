'use strict';

let Belt_Array = require("./Belt_Array.js");
let Primitive_int = require("./Primitive_int.js");

function copyBucket(c) {
  if (c === undefined) {
    return c;
  }
  let head = {
    key: c.key,
    next: undefined
  };
  copyAuxCont(c.next, head);
  return head;
}

function copyAuxCont(_c, _prec) {
  while (true) {
    let prec = _prec;
    let c = _c;
    if (c === undefined) {
      return;
    }
    let ncopy = {
      key: c.key,
      next: undefined
    };
    prec.next = ncopy;
    _prec = ncopy;
    _c = c.next;
    continue;
  };
}

function copyBuckets(buckets) {
  let len = buckets.length;
  let newBuckets = new Array(len);
  for (let i = 0; i < len; ++i) {
    newBuckets[i] = copyBucket(buckets[i]);
  }
  return newBuckets;
}

function copy(x) {
  return {
    size: x.size,
    buckets: copyBuckets(x.buckets),
    hash: x.hash,
    eq: x.eq
  };
}

function bucketLength(_accu, _buckets) {
  while (true) {
    let buckets = _buckets;
    let accu = _accu;
    if (buckets === undefined) {
      return accu;
    }
    _buckets = buckets.next;
    _accu = accu + 1 | 0;
    continue;
  };
}

function doBucketIter(f, _buckets) {
  while (true) {
    let buckets = _buckets;
    if (buckets === undefined) {
      return;
    }
    f(buckets.key);
    _buckets = buckets.next;
    continue;
  };
}

function forEach(h, f) {
  let d = h.buckets;
  for (let i = 0, i_finish = d.length; i < i_finish; ++i) {
    doBucketIter(f, d[i]);
  }
}

function fillArray(_i, arr, _cell) {
  while (true) {
    let cell = _cell;
    let i = _i;
    arr[i] = cell.key;
    let v = cell.next;
    if (v === undefined) {
      return i + 1 | 0;
    }
    _cell = v;
    _i = i + 1 | 0;
    continue;
  };
}

function toArray(h) {
  let d = h.buckets;
  let current = 0;
  let arr = new Array(h.size);
  for (let i = 0, i_finish = d.length; i < i_finish; ++i) {
    let cell = d[i];
    if (cell !== undefined) {
      current = fillArray(current, arr, cell);
    }
  }
  return arr;
}

function doBucketFold(f, _b, _accu) {
  while (true) {
    let accu = _accu;
    let b = _b;
    if (b === undefined) {
      return accu;
    }
    _accu = f(accu, b.key);
    _b = b.next;
    continue;
  };
}

function reduce(h, init, f) {
  let d = h.buckets;
  let accu = init;
  for (let i = 0, i_finish = d.length; i < i_finish; ++i) {
    accu = doBucketFold(f, d[i], accu);
  }
  return accu;
}

function getMaxBucketLength(h) {
  return Belt_Array.reduce(h.buckets, 0, (m, b) => {
    let len = bucketLength(0, b);
    return Primitive_int.max(m, len);
  });
}

function getBucketHistogram(h) {
  let mbl = getMaxBucketLength(h);
  let histo = Belt_Array.makeBy(mbl + 1 | 0, param => 0);
  Belt_Array.forEach(h.buckets, b => {
    let l = bucketLength(0, b);
    histo[l] = histo[l] + 1 | 0;
  });
  return histo;
}

function logStats(h) {
  let histogram = getBucketHistogram(h);
  console.log({
    bindings: h.size,
    buckets: h.buckets.length,
    histogram: histogram
  });
}

let C;

exports.C = C;
exports.copy = copy;
exports.forEach = forEach;
exports.fillArray = fillArray;
exports.toArray = toArray;
exports.reduce = reduce;
exports.logStats = logStats;
exports.getBucketHistogram = getBucketHistogram;
/* No side effect */



import * as Primitive_option from "./Primitive_option.js";
import * as Belt_internalBuckets from "./Belt_internalBuckets.js";
import * as Belt_internalBucketsType from "./Belt_internalBucketsType.js";

function size(h) {
  return h.size;
}

function copyBucketReHash(hash, h_buckets, ndata_tail, _old_bucket) {
  while (true) {
    let old_bucket = _old_bucket;
    if (old_bucket === undefined) {
      return;
    }
    let nidx = hash(old_bucket.key) & (h_buckets.length - 1 | 0);
    let tail = ndata_tail[nidx];
    if (tail !== undefined) {
      tail.next = old_bucket;
    } else {
      h_buckets[nidx] = old_bucket;
    }
    ndata_tail[nidx] = old_bucket;
    _old_bucket = old_bucket.next;
    continue;
  };
}

function replaceInBucket(eq, key, info, _cell) {
  while (true) {
    let cell = _cell;
    if (eq(cell.key, key)) {
      cell.value = info;
      return false;
    }
    let cell$1 = cell.next;
    if (cell$1 === undefined) {
      return true;
    }
    _cell = cell$1;
    continue;
  };
}

function set0(h, key, value, eq, hash) {
  let h_buckets = h.buckets;
  let buckets_len = h_buckets.length;
  let i = hash(key) & (buckets_len - 1 | 0);
  let l = h_buckets[i];
  if (l !== undefined) {
    if (replaceInBucket(eq, key, value, l)) {
      h_buckets[i] = {
        key: key,
        value: value,
        next: l
      };
      h.size = h.size + 1 | 0;
    }
  } else {
    h_buckets[i] = {
      key: key,
      value: value,
      next: undefined
    };
    h.size = h.size + 1 | 0;
  }
  if (h.size > (buckets_len << 1)) {
    let odata = h.buckets;
    let osize = odata.length;
    let nsize = (osize << 1);
    if (nsize < osize) {
      return;
    }
    let h_buckets$1 = new Array(nsize);
    let ndata_tail = new Array(nsize);
    h.buckets = h_buckets$1;
    for (let i$1 = 0; i$1 < osize; ++i$1) {
      copyBucketReHash(hash, h_buckets$1, ndata_tail, odata[i$1]);
    }
    for (let i$2 = 0; i$2 < nsize; ++i$2) {
      let tail = ndata_tail[i$2];
      if (tail !== undefined) {
        tail.next = undefined;
      }
    }
    return;
  }
}

function set(h, key, value) {
  set0(h, key, value, h.eq, h.hash);
}

function remove(h, key) {
  let h_buckets = h.buckets;
  let i = h.hash(key) & (h_buckets.length - 1 | 0);
  let bucket = h_buckets[i];
  if (bucket === undefined) {
    return;
  }
  let eq = h.eq;
  if (eq(bucket.key, key)) {
    h_buckets[i] = bucket.next;
    h.size = h.size - 1 | 0;
    return;
  } else {
    let _prec = bucket;
    let _bucket = bucket.next;
    while (true) {
      let bucket$1 = _bucket;
      let prec = _prec;
      if (bucket$1 === undefined) {
        return;
      }
      let cell_next = bucket$1.next;
      if (eq(bucket$1.key, key)) {
        prec.next = cell_next;
        h.size = h.size - 1 | 0;
        return;
      }
      _bucket = cell_next;
      _prec = bucket$1;
      continue;
    };
  }
}

function get(h, key) {
  let h_buckets = h.buckets;
  let nid = h.hash(key) & (h_buckets.length - 1 | 0);
  let cell1 = h_buckets[nid];
  if (cell1 === undefined) {
    return;
  }
  let eq = h.eq;
  if (eq(key, cell1.key)) {
    return Primitive_option.some(cell1.value);
  }
  let cell2 = cell1.next;
  if (cell2 === undefined) {
    return;
  }
  if (eq(key, cell2.key)) {
    return Primitive_option.some(cell2.value);
  }
  let cell3 = cell2.next;
  if (cell3 !== undefined) {
    if (eq(key, cell3.key)) {
      return Primitive_option.some(cell3.value);
    } else {
      let _buckets = cell3.next;
      while (true) {
        let buckets = _buckets;
        if (buckets === undefined) {
          return;
        }
        if (eq(key, buckets.key)) {
          return Primitive_option.some(buckets.value);
        }
        _buckets = buckets.next;
        continue;
      };
    }
  }
}

function has(h, key) {
  let h_buckets = h.buckets;
  let nid = h.hash(key) & (h_buckets.length - 1 | 0);
  let bucket = h_buckets[nid];
  if (bucket !== undefined) {
    let _cell = bucket;
    let eq = h.eq;
    while (true) {
      let cell = _cell;
      if (eq(cell.key, key)) {
        return true;
      }
      let nextCell = cell.next;
      if (nextCell === undefined) {
        return false;
      }
      _cell = nextCell;
      continue;
    };
  } else {
    return false;
  }
}

function make(hintSize, id) {
  return Belt_internalBucketsType.make(id.hash, id.eq, hintSize);
}

function fromArray(arr, id) {
  let hash = id.hash;
  let eq = id.eq;
  let len = arr.length;
  let v = Belt_internalBucketsType.make(hash, eq, len);
  for (let i = 0; i < len; ++i) {
    let match = arr[i];
    set0(v, match[0], match[1], eq, hash);
  }
  return v;
}

function mergeMany(h, arr) {
  let hash = h.hash;
  let eq = h.eq;
  let len = arr.length;
  for (let i = 0; i < len; ++i) {
    let match = arr[i];
    set0(h, match[0], match[1], eq, hash);
  }
}

let Int;

let $$String;

let clear = Belt_internalBucketsType.clear;

let isEmpty = Belt_internalBucketsType.isEmpty;

let copy = Belt_internalBuckets.copy;

let forEachU = Belt_internalBuckets.forEach;

let forEach = Belt_internalBuckets.forEach;

let reduceU = Belt_internalBuckets.reduce;

let reduce = Belt_internalBuckets.reduce;

let keepMapInPlaceU = Belt_internalBuckets.keepMapInPlace;

let keepMapInPlace = Belt_internalBuckets.keepMapInPlace;

let toArray = Belt_internalBuckets.toArray;

let keysToArray = Belt_internalBuckets.keysToArray;

let valuesToArray = Belt_internalBuckets.valuesToArray;

let getBucketHistogram = Belt_internalBuckets.getBucketHistogram;

let logStats = Belt_internalBuckets.logStats;

export {
  Int,
  $$String,
  make,
  clear,
  isEmpty,
  set,
  copy,
  get,
  has,
  remove,
  forEachU,
  forEach,
  reduceU,
  reduce,
  keepMapInPlaceU,
  keepMapInPlace,
  size,
  toArray,
  keysToArray,
  valuesToArray,
  fromArray,
  mergeMany,
  getBucketHistogram,
  logStats,
}
/* No side effect */

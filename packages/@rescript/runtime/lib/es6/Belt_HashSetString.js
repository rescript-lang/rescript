

import * as Primitive_hash from "./Primitive_hash.js";
import * as Belt_internalSetBuckets from "./Belt_internalSetBuckets.js";
import * as Belt_internalBucketsType from "./Belt_internalBucketsType.js";

function copyBucket(h_buckets, ndata_tail, _old_bucket) {
  while (true) {
    let old_bucket = _old_bucket;
    if (old_bucket === undefined) {
      return;
    }
    let nidx = Primitive_hash.hash_final_mix(Primitive_hash.hash_mix_string(0, old_bucket.key)) & (h_buckets.length - 1 | 0);
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

function remove(h, key) {
  let h_buckets = h.buckets;
  let i = Primitive_hash.hash_final_mix(Primitive_hash.hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
  let l = h_buckets[i];
  if (l === undefined) {
    return;
  }
  let next_cell = l.next;
  if (l.key === key) {
    h.size = h.size - 1 | 0;
    h_buckets[i] = next_cell;
    return;
  } else if (next_cell !== undefined) {
    let _prec = l;
    let _cell = next_cell;
    while (true) {
      let cell = _cell;
      let prec = _prec;
      let cell_next = cell.next;
      if (cell.key === key) {
        prec.next = cell_next;
        h.size = h.size - 1 | 0;
        return;
      }
      if (cell_next === undefined) {
        return;
      }
      _cell = cell_next;
      _prec = cell;
      continue;
    };
  } else {
    return;
  }
}

function addBucket(h, key, _cell) {
  while (true) {
    let cell = _cell;
    if (cell.key === key) {
      return;
    }
    let n = cell.next;
    if (n !== undefined) {
      _cell = n;
      continue;
    }
    h.size = h.size + 1 | 0;
    cell.next = {
      key: key,
      next: undefined
    };
    return;
  };
}

function add(h, key) {
  let h_buckets = h.buckets;
  let buckets_len = h_buckets.length;
  let i = Primitive_hash.hash_final_mix(Primitive_hash.hash_mix_string(0, key)) & (buckets_len - 1 | 0);
  let l = h_buckets[i];
  if (l !== undefined) {
    addBucket(h, key, l);
  } else {
    h_buckets[i] = {
      key: key,
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
      copyBucket(h_buckets$1, ndata_tail, odata[i$1]);
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

function has(h, key) {
  let h_buckets = h.buckets;
  let nid = Primitive_hash.hash_final_mix(Primitive_hash.hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
  let bucket = h_buckets[nid];
  if (bucket !== undefined) {
    let _cell = bucket;
    while (true) {
      let cell = _cell;
      if (cell.key === key) {
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

function make(hintSize) {
  return Belt_internalBucketsType.make(undefined, undefined, hintSize);
}

function size(h) {
  return h.size;
}

function fromArray(arr) {
  let len = arr.length;
  let v = Belt_internalBucketsType.make(undefined, undefined, len);
  for (let i = 0; i < len; ++i) {
    add(v, arr[i]);
  }
  return v;
}

function mergeMany(h, arr) {
  let len = arr.length;
  for (let i = 0; i < len; ++i) {
    add(h, arr[i]);
  }
}

let clear = Belt_internalBucketsType.clear;

let isEmpty = Belt_internalBucketsType.isEmpty;

let copy = Belt_internalSetBuckets.copy;

let forEachU = Belt_internalSetBuckets.forEach;

let forEach = Belt_internalSetBuckets.forEach;

let reduceU = Belt_internalSetBuckets.reduce;

let reduce = Belt_internalSetBuckets.reduce;

let logStats = Belt_internalSetBuckets.logStats;

let toArray = Belt_internalSetBuckets.toArray;

let getBucketHistogram = Belt_internalSetBuckets.getBucketHistogram;

export {
  make,
  clear,
  isEmpty,
  add,
  copy,
  has,
  remove,
  forEachU,
  forEach,
  reduceU,
  reduce,
  size,
  logStats,
  toArray,
  fromArray,
  mergeMany,
  getBucketHistogram,
}
/* No side effect */

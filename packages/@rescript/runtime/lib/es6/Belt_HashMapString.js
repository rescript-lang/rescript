

import * as Primitive_hash from "./Primitive_hash.js";
import * as Primitive_option from "./Primitive_option.js";
import * as Belt_internalBuckets from "./Belt_internalBuckets.js";
import * as Belt_internalBucketsType from "./Belt_internalBucketsType.js";

function copyBucketReHash(h_buckets, ndata_tail, _old_bucket) {
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

function replaceInBucket(key, info, _cell) {
  while (true) {
    let cell = _cell;
    if (cell.key === key) {
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

function set(h, key, value) {
  let h_buckets = h.buckets;
  let buckets_len = h_buckets.length;
  let i = Primitive_hash.hash_final_mix(Primitive_hash.hash_mix_string(0, key)) & (buckets_len - 1 | 0);
  let l = h_buckets[i];
  if (l !== undefined) {
    if (replaceInBucket(key, value, l)) {
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
      copyBucketReHash(h_buckets$1, ndata_tail, odata[i$1]);
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

function remove(h, key) {
  let h_buckets = h.buckets;
  let i = Primitive_hash.hash_final_mix(Primitive_hash.hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
  let bucket = h_buckets[i];
  if (bucket !== undefined) {
    if (bucket.key === key) {
      h_buckets[i] = bucket.next;
      h.size = h.size - 1 | 0;
      return;
    } else {
      let _prec = bucket;
      let _buckets = bucket.next;
      while (true) {
        let buckets = _buckets;
        let prec = _prec;
        if (buckets === undefined) {
          return;
        }
        let cell_next = buckets.next;
        if (buckets.key === key) {
          prec.next = cell_next;
          h.size = h.size - 1 | 0;
          return;
        }
        _buckets = cell_next;
        _prec = buckets;
        continue;
      };
    }
  }
}

function get(h, key) {
  let h_buckets = h.buckets;
  let nid = Primitive_hash.hash_final_mix(Primitive_hash.hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
  let cell1 = h_buckets[nid];
  if (cell1 === undefined) {
    return;
  }
  if (key === cell1.key) {
    return Primitive_option.some(cell1.value);
  }
  let cell2 = cell1.next;
  if (cell2 === undefined) {
    return;
  }
  if (key === cell2.key) {
    return Primitive_option.some(cell2.value);
  }
  let cell3 = cell2.next;
  if (cell3 !== undefined) {
    if (key === cell3.key) {
      return Primitive_option.some(cell3.value);
    } else {
      let _buckets = cell3.next;
      while (true) {
        let buckets = _buckets;
        if (buckets === undefined) {
          return;
        }
        if (key === buckets.key) {
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
    let match = arr[i];
    set(v, match[0], match[1]);
  }
  return v;
}

function mergeMany(h, arr) {
  let len = arr.length;
  for (let i = 0; i < len; ++i) {
    let match = arr[i];
    set(h, match[0], match[1]);
  }
}

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

'use strict';

let Belt_Array = require("./belt_Array.js");
let Caml_option = require("./caml_option.js");

function copyBucket(c) {
  if (c === undefined) {
    return c;
  }
  let head = {
    key: c.key,
    value: c.value,
    next: undefined
  };
  copyAuxCont(c.next, head);
  return head;
}

function copyAuxCont(_c, _prec) {
  while(true) {
    let prec = _prec;
    let c = _c;
    if (c === undefined) {
      return;
    }
    let ncopy = {
      key: c.key,
      value: c.value,
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
  for(let i = 0; i < len; ++i){
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
  while(true) {
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

function do_bucket_iter(f, _buckets) {
  while(true) {
    let buckets = _buckets;
    if (buckets === undefined) {
      return;
    }
    f(buckets.key, buckets.value);
    _buckets = buckets.next;
    continue;
  };
}

function forEachU(h, f) {
  let d = h.buckets;
  for(let i = 0 ,i_finish = d.length; i < i_finish; ++i){
    do_bucket_iter(f, d[i]);
  }
}

function forEach(h, f) {
  forEachU(h, (function (a, b) {
    return f(a, b);
  }));
}

function do_bucket_fold(f, _b, _accu) {
  while(true) {
    let accu = _accu;
    let b = _b;
    if (b === undefined) {
      return accu;
    }
    _accu = f(accu, b.key, b.value);
    _b = b.next;
    continue;
  };
}

function reduceU(h, init, f) {
  let d = h.buckets;
  let accu = init;
  for(let i = 0 ,i_finish = d.length; i < i_finish; ++i){
    accu = do_bucket_fold(f, d[i], accu);
  }
  return accu;
}

function reduce(h, init, f) {
  return reduceU(h, init, (function (a, b, c) {
    return f(a, b, c);
  }));
}

function getMaxBucketLength(h) {
  return Belt_Array.reduceU(h.buckets, 0, (function (m, b) {
    let len = bucketLength(0, b);
    if (m > len) {
      return m;
    } else {
      return len;
    }
  }));
}

function getBucketHistogram(h) {
  let mbl = getMaxBucketLength(h);
  let histo = Belt_Array.makeByU(mbl + 1 | 0, (function (param) {
    return 0;
  }));
  Belt_Array.forEachU(h.buckets, (function (b) {
    let l = bucketLength(0, b);
    histo[l] = histo[l] + 1 | 0;
  }));
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

function filterMapInplaceBucket(f, h, i, _prec, _cell) {
  while(true) {
    let cell = _cell;
    let prec = _prec;
    let n = cell.next;
    let data = f(cell.key, cell.value);
    if (data !== undefined) {
      if (prec !== undefined) {
        cell.next = cell;
      } else {
        h.buckets[i] = cell;
      }
      cell.value = Caml_option.valFromOption(data);
      if (n === undefined) {
        cell.next = n;
        return;
      }
      _cell = n;
      _prec = cell;
      continue;
    }
    h.size = h.size - 1 | 0;
    if (n === undefined) {
      if (prec !== undefined) {
        prec.next = n;
      } else {
        h.buckets[i] = prec;
      }
      return;
    }
    _cell = n;
    continue;
  };
}

function keepMapInPlaceU(h, f) {
  let h_buckets = h.buckets;
  for(let i = 0 ,i_finish = h_buckets.length; i < i_finish; ++i){
    let v = h_buckets[i];
    if (v !== undefined) {
      filterMapInplaceBucket(f, h, i, undefined, v);
    }
    
  }
}

function keepMapInPlace(h, f) {
  keepMapInPlaceU(h, (function (a, b) {
    return f(a, b);
  }));
}

function fillArray(_i, arr, _cell) {
  while(true) {
    let cell = _cell;
    let i = _i;
    arr[i] = [
      cell.key,
      cell.value
    ];
    let v = cell.next;
    if (v === undefined) {
      return i + 1 | 0;
    }
    _cell = v;
    _i = i + 1 | 0;
    continue;
  };
}

function fillArrayMap(_i, arr, _cell, f) {
  while(true) {
    let cell = _cell;
    let i = _i;
    arr[i] = f(cell);
    let v = cell.next;
    if (v === undefined) {
      return i + 1 | 0;
    }
    _cell = v;
    _i = i + 1 | 0;
    continue;
  };
}

function linear(h, f) {
  let d = h.buckets;
  let current = 0;
  let arr = new Array(h.size);
  for(let i = 0 ,i_finish = d.length; i < i_finish; ++i){
    let cell = d[i];
    if (cell !== undefined) {
      current = fillArrayMap(current, arr, cell, f);
    }
    
  }
  return arr;
}

function keysToArray(h) {
  return linear(h, (function (x) {
    return x.key;
  }));
}

function valuesToArray(h) {
  return linear(h, (function (x) {
    return x.value;
  }));
}

function toArray(h) {
  return linear(h, (function (x) {
    return [
      x.key,
      x.value
    ];
  }));
}

let C;

exports.C = C;
exports.copy = copy;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.logStats = logStats;
exports.keepMapInPlaceU = keepMapInPlaceU;
exports.keepMapInPlace = keepMapInPlace;
exports.fillArray = fillArray;
exports.keysToArray = keysToArray;
exports.valuesToArray = valuesToArray;
exports.toArray = toArray;
exports.getBucketHistogram = getBucketHistogram;
/* No side effect */

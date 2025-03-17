'use strict';

let Primitive_option = require("./Primitive_option.js");

function make(length, x) {
  if (length <= 0) {
    return [];
  }
  let arr = new Array(length);
  arr.fill(x);
  return arr;
}

function fromInitializer(length, f) {
  if (length <= 0) {
    return [];
  }
  let arr = new Array(length);
  for (let i = 0; i < length; ++i) {
    arr[i] = f(i);
  }
  return arr;
}

function equal(a, b, eq) {
  let len = a.length;
  if (len === b.length) {
    let _i = 0;
    while (true) {
      let i = _i;
      if (i === len) {
        return true;
      }
      if (!eq(a[i], b[i])) {
        return false;
      }
      _i = i + 1 | 0;
      continue;
    };
  } else {
    return false;
  }
}

function compare(a, b, cmp) {
  let lenA = a.length;
  let lenB = b.length;
  if (lenA < lenB) {
    return -1;
  } else if (lenA > lenB) {
    return 1;
  } else {
    let _i = 0;
    while (true) {
      let i = _i;
      if (i === lenA) {
        return 0;
      }
      let c = cmp(a[i], b[i]);
      if (c !== 0) {
        return c;
      }
      _i = i + 1 | 0;
      continue;
    };
  }
}

function indexOfOpt(arr, item) {
  let index = arr.indexOf(item);
  if (index !== -1) {
    return index;
  }
  
}

function lastIndexOfOpt(arr, item) {
  let index = arr.lastIndexOf(item);
  if (index !== -1) {
    return index;
  }
  
}

function reduce(arr, init, f) {
  return arr.reduce(f, init);
}

function reduceWithIndex(arr, init, f) {
  return arr.reduce(f, init);
}

function reduceRight(arr, init, f) {
  return arr.reduceRight(f, init);
}

function reduceRightWithIndex(arr, init, f) {
  return arr.reduceRight(f, init);
}

function findIndexOpt(array, finder) {
  let index = array.findIndex(finder);
  if (index !== -1) {
    return index;
  }
  
}

function swapUnsafe(xs, i, j) {
  let tmp = xs[i];
  xs[i] = xs[j];
  xs[j] = tmp;
}

function random_int(min, max) {
  return (Math.floor(Math.random() * (max - min | 0)) | 0) + min | 0;
}

function shuffle(xs) {
  let len = xs.length;
  for (let i = 0; i < len; ++i) {
    swapUnsafe(xs, i, random_int(i, len));
  }
}

function toShuffled(xs) {
  let result = xs.slice();
  shuffle(result);
  return result;
}

function filterMap(a, f) {
  let l = a.length;
  let r = new Array(l);
  let j = 0;
  for (let i = 0; i < l; ++i) {
    let v = a[i];
    let v$1 = f(v);
    if (v$1 !== undefined) {
      r[j] = Primitive_option.valFromOption(v$1);
      j = j + 1 | 0;
    }
    
  }
  r.length = j;
  return r;
}

function keepSome(__x) {
  return filterMap(__x, x => x);
}

function findMap(arr, f) {
  let _i = 0;
  while (true) {
    let i = _i;
    if (i === arr.length) {
      return;
    }
    let r = f(arr[i]);
    if (r !== undefined) {
      return r;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function last(a) {
  return a[a.length - 1 | 0];
}

exports.make = make;
exports.fromInitializer = fromInitializer;
exports.equal = equal;
exports.compare = compare;
exports.indexOfOpt = indexOfOpt;
exports.lastIndexOfOpt = lastIndexOfOpt;
exports.reduce = reduce;
exports.reduceWithIndex = reduceWithIndex;
exports.reduceRight = reduceRight;
exports.reduceRightWithIndex = reduceRightWithIndex;
exports.findIndexOpt = findIndexOpt;
exports.filterMap = filterMap;
exports.keepSome = keepSome;
exports.toShuffled = toShuffled;
exports.shuffle = shuffle;
exports.findMap = findMap;
exports.last = last;
/* No side effect */

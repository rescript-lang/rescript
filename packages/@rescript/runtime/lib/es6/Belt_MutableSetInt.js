

import * as Primitive_int from "./Primitive_int.js";
import * as Belt_SortArrayInt from "./Belt_SortArrayInt.js";
import * as Belt_internalAVLset from "./Belt_internalAVLset.js";
import * as Belt_internalSetInt from "./Belt_internalSetInt.js";

function remove0(nt, x) {
  let k = nt.v;
  if (x === k) {
    let l = nt.l;
    let r = nt.r;
    if (l !== undefined) {
      if (r !== undefined) {
        nt.r = Belt_internalAVLset.removeMinAuxWithRootMutate(nt, r);
        return Belt_internalAVLset.balMutate(nt);
      } else {
        return l;
      }
    } else {
      return r;
    }
  }
  if (x < k) {
    let l$1 = nt.l;
    if (l$1 !== undefined) {
      nt.l = remove0(l$1, x);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
  let r$1 = nt.r;
  if (r$1 !== undefined) {
    nt.r = remove0(r$1, x);
    return Belt_internalAVLset.balMutate(nt);
  } else {
    return nt;
  }
}

function remove(d, v) {
  let oldRoot = d.data;
  if (oldRoot === undefined) {
    return;
  }
  let newRoot = remove0(oldRoot, v);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return;
  }
}

function removeMany0(_t, xs, _i, len) {
  while (true) {
    let i = _i;
    let t = _t;
    if (i >= len) {
      return t;
    }
    let ele = xs[i];
    let u = remove0(t, ele);
    if (u === undefined) {
      return;
    }
    _i = i + 1 | 0;
    _t = u;
    continue;
  };
}

function removeMany(d, xs) {
  let oldRoot = d.data;
  if (oldRoot === undefined) {
    return;
  }
  let len = xs.length;
  d.data = removeMany0(oldRoot, xs, 0, len);
}

function removeCheck0(nt, x, removed) {
  let k = nt.v;
  if (x === k) {
    removed.contents = true;
    let l = nt.l;
    let r = nt.r;
    if (l !== undefined) {
      if (r !== undefined) {
        nt.r = Belt_internalAVLset.removeMinAuxWithRootMutate(nt, r);
        return Belt_internalAVLset.balMutate(nt);
      } else {
        return l;
      }
    } else {
      return r;
    }
  }
  if (x < k) {
    let l$1 = nt.l;
    if (l$1 !== undefined) {
      nt.l = removeCheck0(l$1, x, removed);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
  let r$1 = nt.r;
  if (r$1 !== undefined) {
    nt.r = removeCheck0(r$1, x, removed);
    return Belt_internalAVLset.balMutate(nt);
  } else {
    return nt;
  }
}

function removeCheck(d, v) {
  let oldRoot = d.data;
  if (oldRoot === undefined) {
    return false;
  }
  let removed = {
    contents: false
  };
  let newRoot = removeCheck0(oldRoot, v, removed);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
  }
  return removed.contents;
}

function addCheck0(t, x, added) {
  if (t !== undefined) {
    let k = t.v;
    if (x === k) {
      return t;
    }
    let l = t.l;
    let r = t.r;
    if (x < k) {
      let ll = addCheck0(l, x, added);
      t.l = ll;
    } else {
      t.r = addCheck0(r, x, added);
    }
    return Belt_internalAVLset.balMutate(t);
  }
  added.contents = true;
  return Belt_internalAVLset.singleton(x);
}

function addCheck(m, e) {
  let oldRoot = m.data;
  let added = {
    contents: false
  };
  let newRoot = addCheck0(oldRoot, e, added);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
  }
  return added.contents;
}

function add(d, k) {
  let oldRoot = d.data;
  let v = Belt_internalSetInt.addMutate(oldRoot, k);
  if (v !== oldRoot) {
    d.data = v;
    return;
  }
}

function addArrayMutate(t, xs) {
  let v = t;
  for (let i = 0, i_finish = xs.length; i < i_finish; ++i) {
    v = Belt_internalSetInt.addMutate(v, xs[i]);
  }
  return v;
}

function mergeMany(d, arr) {
  d.data = addArrayMutate(d.data, arr);
}

function make() {
  return {
    data: undefined
  };
}

function isEmpty(d) {
  let n = d.data;
  return n === undefined;
}

function minimum(d) {
  return Belt_internalAVLset.minimum(d.data);
}

function minUndefined(d) {
  return Belt_internalAVLset.minUndefined(d.data);
}

function maximum(d) {
  return Belt_internalAVLset.maximum(d.data);
}

function maxUndefined(d) {
  return Belt_internalAVLset.maxUndefined(d.data);
}

function forEach(d, f) {
  Belt_internalAVLset.forEach(d.data, f);
}

function reduce(d, acc, cb) {
  return Belt_internalAVLset.reduce(d.data, acc, cb);
}

function every(d, p) {
  return Belt_internalAVLset.every(d.data, p);
}

function some(d, p) {
  return Belt_internalAVLset.some(d.data, p);
}

function size(d) {
  return Belt_internalAVLset.size(d.data);
}

function toList(d) {
  return Belt_internalAVLset.toList(d.data);
}

function toArray(d) {
  return Belt_internalAVLset.toArray(d.data);
}

function fromSortedArrayUnsafe(xs) {
  return {
    data: Belt_internalAVLset.fromSortedArrayUnsafe(xs)
  };
}

function checkInvariantInternal(d) {
  Belt_internalAVLset.checkInvariantInternal(d.data);
}

function fromArray(xs) {
  return {
    data: Belt_internalSetInt.fromArray(xs)
  };
}

function cmp(d0, d1) {
  return Belt_internalSetInt.cmp(d0.data, d1.data);
}

function eq(d0, d1) {
  return Belt_internalSetInt.eq(d0.data, d1.data);
}

function get(d, x) {
  return Belt_internalSetInt.get(d.data, x);
}

function getUndefined(d, x) {
  return Belt_internalSetInt.getUndefined(d.data, x);
}

function getOrThrow(d, x) {
  return Belt_internalSetInt.getOrThrow(d.data, x);
}

function split(d, key) {
  let arr = Belt_internalAVLset.toArray(d.data);
  let i = Belt_SortArrayInt.binarySearch(arr, key);
  let len = arr.length;
  if (i >= 0) {
    return [
      [
        {
          data: Belt_internalAVLset.fromSortedArrayAux(arr, 0, i)
        },
        {
          data: Belt_internalAVLset.fromSortedArrayAux(arr, i + 1 | 0, (len - i | 0) - 1 | 0)
        }
      ],
      true
    ];
  }
  let next = (-i | 0) - 1 | 0;
  return [
    [
      {
        data: Belt_internalAVLset.fromSortedArrayAux(arr, 0, next)
      },
      {
        data: Belt_internalAVLset.fromSortedArrayAux(arr, next, len - next | 0)
      }
    ],
    false
  ];
}

function keep(d, p) {
  return {
    data: Belt_internalAVLset.keepCopy(d.data, p)
  };
}

function partition(d, p) {
  let match = Belt_internalAVLset.partitionCopy(d.data, p);
  return [
    {
      data: match[0]
    },
    {
      data: match[1]
    }
  ];
}

function subset(a, b) {
  return Belt_internalSetInt.subset(a.data, b.data);
}

function intersect(dataa, datab) {
  let dataa$1 = dataa.data;
  let datab$1 = datab.data;
  if (dataa$1 === undefined) {
    return {
      data: undefined
    };
  }
  if (datab$1 === undefined) {
    return {
      data: undefined
    };
  }
  let sizea = Belt_internalAVLset.lengthNode(dataa$1);
  let sizeb = Belt_internalAVLset.lengthNode(datab$1);
  let totalSize = sizea + sizeb | 0;
  let tmp = new Array(totalSize);
  Belt_internalAVLset.fillArray(dataa$1, 0, tmp);
  Belt_internalAVLset.fillArray(datab$1, sizea, tmp);
  if (tmp[sizea - 1 | 0] < tmp[sizea] || tmp[totalSize - 1 | 0] < tmp[0]) {
    return {
      data: undefined
    };
  }
  let tmp2 = new Array(Primitive_int.min(sizea, sizeb));
  let k = Belt_SortArrayInt.intersect(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0);
  return {
    data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
  };
}

function diff(dataa, datab) {
  let dataa$1 = dataa.data;
  let datab$1 = datab.data;
  if (dataa$1 === undefined) {
    return {
      data: undefined
    };
  }
  if (datab$1 === undefined) {
    return {
      data: Belt_internalAVLset.copy(dataa$1)
    };
  }
  let sizea = Belt_internalAVLset.lengthNode(dataa$1);
  let sizeb = Belt_internalAVLset.lengthNode(datab$1);
  let totalSize = sizea + sizeb | 0;
  let tmp = new Array(totalSize);
  Belt_internalAVLset.fillArray(dataa$1, 0, tmp);
  Belt_internalAVLset.fillArray(datab$1, sizea, tmp);
  if (tmp[sizea - 1 | 0] < tmp[sizea] || tmp[totalSize - 1 | 0] < tmp[0]) {
    return {
      data: Belt_internalAVLset.copy(dataa$1)
    };
  }
  let tmp2 = new Array(sizea);
  let k = Belt_SortArrayInt.diff(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0);
  return {
    data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
  };
}

function union(dataa, datab) {
  let dataa$1 = dataa.data;
  let datab$1 = datab.data;
  if (dataa$1 === undefined) {
    return {
      data: Belt_internalAVLset.copy(datab$1)
    };
  }
  if (datab$1 === undefined) {
    return {
      data: Belt_internalAVLset.copy(dataa$1)
    };
  }
  let sizea = Belt_internalAVLset.lengthNode(dataa$1);
  let sizeb = Belt_internalAVLset.lengthNode(datab$1);
  let totalSize = sizea + sizeb | 0;
  let tmp = new Array(totalSize);
  Belt_internalAVLset.fillArray(dataa$1, 0, tmp);
  Belt_internalAVLset.fillArray(datab$1, sizea, tmp);
  if (tmp[sizea - 1 | 0] < tmp[sizea]) {
    return {
      data: Belt_internalAVLset.fromSortedArrayAux(tmp, 0, totalSize)
    };
  }
  let tmp2 = new Array(totalSize);
  let k = Belt_SortArrayInt.union(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0);
  return {
    data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
  };
}

function has(d, x) {
  return Belt_internalSetInt.has(d.data, x);
}

function copy(d) {
  return {
    data: Belt_internalAVLset.copy(d.data)
  };
}

let forEachU = forEach;

let reduceU = reduce;

let everyU = every;

let someU = some;

let keepU = keep;

let partitionU = partition;

let getExn = getOrThrow;

export {
  make,
  fromArray,
  fromSortedArrayUnsafe,
  copy,
  isEmpty,
  has,
  add,
  addCheck,
  mergeMany,
  remove,
  removeCheck,
  removeMany,
  union,
  intersect,
  diff,
  subset,
  cmp,
  eq,
  forEachU,
  forEach,
  reduceU,
  reduce,
  everyU,
  every,
  someU,
  some,
  keepU,
  keep,
  partitionU,
  partition,
  size,
  toList,
  toArray,
  minimum,
  minUndefined,
  maximum,
  maxUndefined,
  get,
  getUndefined,
  getExn,
  getOrThrow,
  split,
  checkInvariantInternal,
}
/* No side effect */

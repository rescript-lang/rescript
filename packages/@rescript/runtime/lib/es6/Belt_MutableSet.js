

import * as Primitive_int from "./Primitive_int.js";
import * as Belt_SortArray from "./Belt_SortArray.js";
import * as Belt_internalAVLset from "./Belt_internalAVLset.js";

function remove0(nt, x, cmp) {
  let k = nt.v;
  let c = cmp(x, k);
  if (c === 0) {
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
  if (c < 0) {
    let l$1 = nt.l;
    if (l$1 !== undefined) {
      nt.l = remove0(l$1, x, cmp);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
  let r$1 = nt.r;
  if (r$1 !== undefined) {
    nt.r = remove0(r$1, x, cmp);
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
  let newRoot = remove0(oldRoot, v, d.cmp);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return;
  }
}

function removeMany0(_t, xs, _i, len, cmp) {
  while (true) {
    let i = _i;
    let t = _t;
    if (i >= len) {
      return t;
    }
    let ele = xs[i];
    let u = remove0(t, ele, cmp);
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
  d.data = removeMany0(oldRoot, xs, 0, len, d.cmp);
}

function removeCheck0(nt, x, removed, cmp) {
  let k = nt.v;
  let c = cmp(x, k);
  if (c === 0) {
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
  if (c < 0) {
    let l$1 = nt.l;
    if (l$1 !== undefined) {
      nt.l = removeCheck0(l$1, x, removed, cmp);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
  let r$1 = nt.r;
  if (r$1 !== undefined) {
    nt.r = removeCheck0(r$1, x, removed, cmp);
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
  let newRoot = removeCheck0(oldRoot, v, removed, d.cmp);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
  }
  return removed.contents;
}

function addCheck0(t, x, added, cmp) {
  if (t !== undefined) {
    let k = t.v;
    let c = cmp(x, k);
    if (c === 0) {
      return t;
    }
    let l = t.l;
    let r = t.r;
    if (c < 0) {
      let ll = addCheck0(l, x, added, cmp);
      t.l = ll;
    } else {
      t.r = addCheck0(r, x, added, cmp);
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
  let newRoot = addCheck0(oldRoot, e, added, m.cmp);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
  }
  return added.contents;
}

function add(m, e) {
  let oldRoot = m.data;
  let newRoot = Belt_internalAVLset.addMutate(m.cmp, oldRoot, e);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
    return;
  }
}

function addArrayMutate(t, xs, cmp) {
  let v = t;
  for (let i = 0, i_finish = xs.length; i < i_finish; ++i) {
    v = Belt_internalAVLset.addMutate(cmp, v, xs[i]);
  }
  return v;
}

function mergeMany(d, xs) {
  d.data = addArrayMutate(d.data, xs, d.cmp);
}

function make(id) {
  return {
    cmp: id.cmp,
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

function fromSortedArrayUnsafe(xs, id) {
  return {
    cmp: id.cmp,
    data: Belt_internalAVLset.fromSortedArrayUnsafe(xs)
  };
}

function checkInvariantInternal(d) {
  Belt_internalAVLset.checkInvariantInternal(d.data);
}

function fromArray(data, id) {
  let cmp = id.cmp;
  return {
    cmp: cmp,
    data: Belt_internalAVLset.fromArray(data, cmp)
  };
}

function cmp(d0, d1) {
  return Belt_internalAVLset.cmp(d0.data, d1.data, d0.cmp);
}

function eq(d0, d1) {
  return Belt_internalAVLset.eq(d0.data, d1.data, d0.cmp);
}

function get(d, x) {
  return Belt_internalAVLset.get(d.data, x, d.cmp);
}

function getUndefined(d, x) {
  return Belt_internalAVLset.getUndefined(d.data, x, d.cmp);
}

function getOrThrow(d, x) {
  return Belt_internalAVLset.getOrThrow(d.data, x, d.cmp);
}

function split(d, key) {
  let arr = Belt_internalAVLset.toArray(d.data);
  let cmp = d.cmp;
  let i = Belt_SortArray.binarySearchBy(arr, key, cmp);
  let len = arr.length;
  if (i >= 0) {
    return [
      [
        {
          cmp: cmp,
          data: Belt_internalAVLset.fromSortedArrayAux(arr, 0, i)
        },
        {
          cmp: cmp,
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
        cmp: cmp,
        data: Belt_internalAVLset.fromSortedArrayAux(arr, 0, next)
      },
      {
        cmp: cmp,
        data: Belt_internalAVLset.fromSortedArrayAux(arr, next, len - next | 0)
      }
    ],
    false
  ];
}

function keep(d, p) {
  return {
    cmp: d.cmp,
    data: Belt_internalAVLset.keepCopy(d.data, p)
  };
}

function partition(d, p) {
  let cmp = d.cmp;
  let match = Belt_internalAVLset.partitionCopy(d.data, p);
  return [
    {
      cmp: cmp,
      data: match[0]
    },
    {
      cmp: cmp,
      data: match[1]
    }
  ];
}

function subset(a, b) {
  return Belt_internalAVLset.subset(a.data, b.data, a.cmp);
}

function intersect(a, b) {
  let cmp = a.cmp;
  let match = a.data;
  let match$1 = b.data;
  if (match === undefined) {
    return {
      cmp: cmp,
      data: undefined
    };
  }
  if (match$1 === undefined) {
    return {
      cmp: cmp,
      data: undefined
    };
  }
  let sizea = Belt_internalAVLset.lengthNode(match);
  let sizeb = Belt_internalAVLset.lengthNode(match$1);
  let totalSize = sizea + sizeb | 0;
  let tmp = new Array(totalSize);
  Belt_internalAVLset.fillArray(match, 0, tmp);
  Belt_internalAVLset.fillArray(match$1, sizea, tmp);
  if (cmp(tmp[sizea - 1 | 0], tmp[sizea]) < 0 || cmp(tmp[totalSize - 1 | 0], tmp[0]) < 0) {
    return {
      cmp: cmp,
      data: undefined
    };
  }
  let tmp2 = new Array(Primitive_int.min(sizea, sizeb));
  let k = Belt_SortArray.intersect(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, cmp);
  return {
    cmp: cmp,
    data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
  };
}

function diff(a, b) {
  let cmp = a.cmp;
  let dataa = a.data;
  let match = b.data;
  if (dataa === undefined) {
    return {
      cmp: cmp,
      data: undefined
    };
  }
  if (match === undefined) {
    return {
      cmp: cmp,
      data: Belt_internalAVLset.copy(dataa)
    };
  }
  let sizea = Belt_internalAVLset.lengthNode(dataa);
  let sizeb = Belt_internalAVLset.lengthNode(match);
  let totalSize = sizea + sizeb | 0;
  let tmp = new Array(totalSize);
  Belt_internalAVLset.fillArray(dataa, 0, tmp);
  Belt_internalAVLset.fillArray(match, sizea, tmp);
  if (cmp(tmp[sizea - 1 | 0], tmp[sizea]) < 0 || cmp(tmp[totalSize - 1 | 0], tmp[0]) < 0) {
    return {
      cmp: cmp,
      data: Belt_internalAVLset.copy(dataa)
    };
  }
  let tmp2 = new Array(sizea);
  let k = Belt_SortArray.diff(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, cmp);
  return {
    cmp: cmp,
    data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
  };
}

function union(a, b) {
  let cmp = a.cmp;
  let dataa = a.data;
  let datab = b.data;
  if (dataa === undefined) {
    return {
      cmp: cmp,
      data: Belt_internalAVLset.copy(datab)
    };
  }
  if (datab === undefined) {
    return {
      cmp: cmp,
      data: Belt_internalAVLset.copy(dataa)
    };
  }
  let sizea = Belt_internalAVLset.lengthNode(dataa);
  let sizeb = Belt_internalAVLset.lengthNode(datab);
  let totalSize = sizea + sizeb | 0;
  let tmp = new Array(totalSize);
  Belt_internalAVLset.fillArray(dataa, 0, tmp);
  Belt_internalAVLset.fillArray(datab, sizea, tmp);
  if (cmp(tmp[sizea - 1 | 0], tmp[sizea]) < 0) {
    return {
      cmp: cmp,
      data: Belt_internalAVLset.fromSortedArrayAux(tmp, 0, totalSize)
    };
  }
  let tmp2 = new Array(totalSize);
  let k = Belt_SortArray.union(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, cmp);
  return {
    cmp: cmp,
    data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
  };
}

function has(d, x) {
  return Belt_internalAVLset.has(d.data, x, d.cmp);
}

function copy(d) {
  return {
    cmp: d.cmp,
    data: Belt_internalAVLset.copy(d.data)
  };
}

let Int;

let $$String;

let forEachU = forEach;

let reduceU = reduce;

let everyU = every;

let someU = some;

let keepU = keep;

let partitionU = partition;

let getExn = getOrThrow;

export {
  Int,
  $$String,
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

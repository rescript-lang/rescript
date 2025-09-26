

import * as Primitive_option from "./Primitive_option.js";
import * as Belt_internalAVLtree from "./Belt_internalAVLtree.js";

function removeMutateAux(nt, x, cmp) {
  let k = nt.k;
  let c = cmp(x, k);
  if (c === 0) {
    let l = nt.l;
    let r = nt.r;
    if (l !== undefined) {
      if (r !== undefined) {
        nt.r = Belt_internalAVLtree.removeMinAuxWithRootMutate(nt, r);
        return Belt_internalAVLtree.balMutate(nt);
      } else {
        return l;
      }
    } else if (r !== undefined) {
      return r;
    } else {
      return l;
    }
  }
  if (c < 0) {
    let l$1 = nt.l;
    if (l$1 !== undefined) {
      nt.l = removeMutateAux(l$1, x, cmp);
      return Belt_internalAVLtree.balMutate(nt);
    } else {
      return nt;
    }
  }
  let r$1 = nt.r;
  if (r$1 !== undefined) {
    nt.r = removeMutateAux(r$1, x, cmp);
    return Belt_internalAVLtree.balMutate(nt);
  } else {
    return nt;
  }
}

function remove(d, k) {
  let oldRoot = d.data;
  if (oldRoot === undefined) {
    return;
  }
  let newRoot = removeMutateAux(oldRoot, k, d.cmp);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return;
  }
}

function removeArrayMutateAux(_t, xs, _i, len, cmp) {
  while (true) {
    let i = _i;
    let t = _t;
    if (i >= len) {
      return t;
    }
    let ele = xs[i];
    let u = removeMutateAux(t, ele, cmp);
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
  let newRoot = removeArrayMutateAux(oldRoot, xs, 0, len, d.cmp);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return;
  }
}

function updateDone(t, x, f, cmp) {
  if (t !== undefined) {
    let k = t.k;
    let c = cmp(x, k);
    if (c === 0) {
      let data = f(Primitive_option.some(t.v));
      if (data !== undefined) {
        t.v = Primitive_option.valFromOption(data);
        return t;
      }
      let l = t.l;
      let r = t.r;
      if (l !== undefined) {
        if (r !== undefined) {
          t.r = Belt_internalAVLtree.removeMinAuxWithRootMutate(t, r);
          return Belt_internalAVLtree.balMutate(t);
        } else {
          return l;
        }
      } else if (r !== undefined) {
        return r;
      } else {
        return l;
      }
    }
    if (c < 0) {
      t.l = updateDone(t.l, x, f, cmp);
    } else {
      t.r = updateDone(t.r, x, f, cmp);
    }
    return Belt_internalAVLtree.balMutate(t);
  }
  let data$1 = f(undefined);
  if (data$1 !== undefined) {
    return Belt_internalAVLtree.singleton(x, Primitive_option.valFromOption(data$1));
  } else {
    return t;
  }
}

function update(t, x, f) {
  let oldRoot = t.data;
  let newRoot = updateDone(oldRoot, x, f, t.cmp);
  if (newRoot !== oldRoot) {
    t.data = newRoot;
    return;
  }
}

function make(id) {
  return {
    cmp: id.cmp,
    data: undefined
  };
}

function clear(m) {
  m.data = undefined;
}

function isEmpty(d) {
  let x = d.data;
  return x === undefined;
}

function minKey(m) {
  return Belt_internalAVLtree.minKey(m.data);
}

function minKeyUndefined(m) {
  return Belt_internalAVLtree.minKeyUndefined(m.data);
}

function maxKey(m) {
  return Belt_internalAVLtree.maxKey(m.data);
}

function maxKeyUndefined(m) {
  return Belt_internalAVLtree.maxKeyUndefined(m.data);
}

function minimum(m) {
  return Belt_internalAVLtree.minimum(m.data);
}

function minUndefined(m) {
  return Belt_internalAVLtree.minUndefined(m.data);
}

function maximum(m) {
  return Belt_internalAVLtree.maximum(m.data);
}

function maxUndefined(m) {
  return Belt_internalAVLtree.maxUndefined(m.data);
}

function forEach(d, f) {
  Belt_internalAVLtree.forEach(d.data, f);
}

function reduce(d, acc, cb) {
  return Belt_internalAVLtree.reduce(d.data, acc, cb);
}

function every(d, p) {
  return Belt_internalAVLtree.every(d.data, p);
}

function some(d, p) {
  return Belt_internalAVLtree.some(d.data, p);
}

function size(d) {
  return Belt_internalAVLtree.size(d.data);
}

function toList(d) {
  return Belt_internalAVLtree.toList(d.data);
}

function toArray(d) {
  return Belt_internalAVLtree.toArray(d.data);
}

function keysToArray(d) {
  return Belt_internalAVLtree.keysToArray(d.data);
}

function valuesToArray(d) {
  return Belt_internalAVLtree.valuesToArray(d.data);
}

function checkInvariantInternal(d) {
  Belt_internalAVLtree.checkInvariantInternal(d.data);
}

function cmp(m1, m2, cmp$1) {
  return Belt_internalAVLtree.cmp(m1.data, m2.data, m1.cmp, cmp$1);
}

function eq(m1, m2, cmp) {
  return Belt_internalAVLtree.eq(m1.data, m2.data, m1.cmp, cmp);
}

function map(m, f) {
  return {
    cmp: m.cmp,
    data: Belt_internalAVLtree.map(m.data, f)
  };
}

function mapWithKey(m, f) {
  return {
    cmp: m.cmp,
    data: Belt_internalAVLtree.mapWithKey(m.data, f)
  };
}

function get(m, x) {
  return Belt_internalAVLtree.get(m.data, x, m.cmp);
}

function getUndefined(m, x) {
  return Belt_internalAVLtree.getUndefined(m.data, x, m.cmp);
}

function getWithDefault(m, x, def) {
  return Belt_internalAVLtree.getWithDefault(m.data, x, def, m.cmp);
}

function getOrThrow(m, x) {
  return Belt_internalAVLtree.getOrThrow(m.data, x, m.cmp);
}

function has(m, x) {
  return Belt_internalAVLtree.has(m.data, x, m.cmp);
}

function fromArray(data, id) {
  let cmp = id.cmp;
  return {
    cmp: cmp,
    data: Belt_internalAVLtree.fromArray(data, cmp)
  };
}

function set(m, e, v) {
  let oldRoot = m.data;
  let newRoot = Belt_internalAVLtree.updateMutate(oldRoot, e, v, m.cmp);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
    return;
  }
}

function mergeManyAux(t, xs, cmp) {
  let v = t;
  for (let i = 0, i_finish = xs.length; i < i_finish; ++i) {
    let match = xs[i];
    v = Belt_internalAVLtree.updateMutate(v, match[0], match[1], cmp);
  }
  return v;
}

function mergeMany(d, xs) {
  let oldRoot = d.data;
  let newRoot = mergeManyAux(oldRoot, xs, d.cmp);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return;
  }
}

let Int;

let $$String;

let cmpU = cmp;

let eqU = eq;

let forEachU = forEach;

let reduceU = reduce;

let everyU = every;

let someU = some;

let getExn = getOrThrow;

let updateU = update;

let mapU = map;

let mapWithKeyU = mapWithKey;

export {
  Int,
  $$String,
  make,
  clear,
  isEmpty,
  has,
  cmpU,
  cmp,
  eqU,
  eq,
  forEachU,
  forEach,
  reduceU,
  reduce,
  everyU,
  every,
  someU,
  some,
  size,
  toList,
  toArray,
  fromArray,
  keysToArray,
  valuesToArray,
  minKey,
  minKeyUndefined,
  maxKey,
  maxKeyUndefined,
  minimum,
  minUndefined,
  maximum,
  maxUndefined,
  get,
  getUndefined,
  getWithDefault,
  getExn,
  getOrThrow,
  checkInvariantInternal,
  remove,
  removeMany,
  set,
  updateU,
  update,
  mergeMany,
  mapU,
  map,
  mapWithKeyU,
  mapWithKey,
}
/* No side effect */



import * as Primitive_option from "./Primitive_option.js";
import * as Belt_internalAVLtree from "./Belt_internalAVLtree.js";
import * as Belt_internalMapString from "./Belt_internalMapString.js";

function set(t, newK, newD) {
  if (t === undefined) {
    return Belt_internalAVLtree.singleton(newK, newD);
  }
  let k = t.k;
  if (newK === k) {
    return Belt_internalAVLtree.updateValue(t, newD);
  }
  let v = t.v;
  if (newK < k) {
    return Belt_internalAVLtree.bal(set(t.l, newK, newD), k, v, t.r);
  } else {
    return Belt_internalAVLtree.bal(t.l, k, v, set(t.r, newK, newD));
  }
}

function update(t, x, f) {
  if (t !== undefined) {
    let k = t.k;
    if (x === k) {
      let data = f(Primitive_option.some(t.v));
      if (data !== undefined) {
        return Belt_internalAVLtree.updateValue(t, Primitive_option.valFromOption(data));
      }
      let l = t.l;
      let r = t.r;
      if (l === undefined) {
        return r;
      }
      if (r === undefined) {
        return l;
      }
      let kr = {
        contents: r.k
      };
      let vr = {
        contents: r.v
      };
      let r$1 = Belt_internalAVLtree.removeMinAuxWithRef(r, kr, vr);
      return Belt_internalAVLtree.bal(l, kr.contents, vr.contents, r$1);
    }
    let v = t.v;
    let l$1 = t.l;
    let r$2 = t.r;
    if (x < k) {
      let ll = update(l$1, x, f);
      if (l$1 === ll) {
        return t;
      } else {
        return Belt_internalAVLtree.bal(ll, k, v, r$2);
      }
    }
    let rr = update(r$2, x, f);
    if (r$2 === rr) {
      return t;
    } else {
      return Belt_internalAVLtree.bal(l$1, k, v, rr);
    }
  }
  let data$1 = f(undefined);
  if (data$1 !== undefined) {
    return Belt_internalAVLtree.singleton(x, Primitive_option.valFromOption(data$1));
  } else {
    return t;
  }
}

function removeAux(n, x) {
  let v = n.k;
  let l = n.l;
  let r = n.r;
  if (x === v) {
    if (l === undefined) {
      return r;
    }
    if (r === undefined) {
      return l;
    }
    let kr = {
      contents: r.k
    };
    let vr = {
      contents: r.v
    };
    let r$1 = Belt_internalAVLtree.removeMinAuxWithRef(r, kr, vr);
    return Belt_internalAVLtree.bal(l, kr.contents, vr.contents, r$1);
  }
  if (x < v) {
    if (l === undefined) {
      return n;
    }
    let ll = removeAux(l, x);
    if (ll === l) {
      return n;
    } else {
      return Belt_internalAVLtree.bal(ll, v, n.v, r);
    }
  }
  if (r === undefined) {
    return n;
  }
  let rr = removeAux(r, x);
  return Belt_internalAVLtree.bal(l, v, n.v, rr);
}

function remove(n, x) {
  if (n !== undefined) {
    return removeAux(n, x);
  }
}

function removeMany(t, keys) {
  let len = keys.length;
  if (t !== undefined) {
    let _t = t;
    let _i = 0;
    while (true) {
      let i = _i;
      let t$1 = _t;
      if (i >= len) {
        return t$1;
      }
      let ele = keys[i];
      let u = removeAux(t$1, ele);
      if (u === undefined) {
        return u;
      }
      _i = i + 1 | 0;
      _t = u;
      continue;
    };
  }
}

function mergeMany(h, arr) {
  let len = arr.length;
  let v = h;
  for (let i = 0; i < len; ++i) {
    let match = arr[i];
    v = set(v, match[0], match[1]);
  }
  return v;
}

let empty;

let isEmpty = Belt_internalAVLtree.isEmpty;

let has = Belt_internalMapString.has;

let cmpU = Belt_internalMapString.cmp;

let cmp = Belt_internalMapString.cmp;

let eqU = Belt_internalMapString.eq;

let eq = Belt_internalMapString.eq;

let findFirstByU = Belt_internalAVLtree.findFirstBy;

let findFirstBy = Belt_internalAVLtree.findFirstBy;

let forEachU = Belt_internalAVLtree.forEach;

let forEach = Belt_internalAVLtree.forEach;

let reduceU = Belt_internalAVLtree.reduce;

let reduce = Belt_internalAVLtree.reduce;

let everyU = Belt_internalAVLtree.every;

let every = Belt_internalAVLtree.every;

let someU = Belt_internalAVLtree.some;

let some = Belt_internalAVLtree.some;

let size = Belt_internalAVLtree.size;

let toList = Belt_internalAVLtree.toList;

let toArray = Belt_internalAVLtree.toArray;

let fromArray = Belt_internalMapString.fromArray;

let keysToArray = Belt_internalAVLtree.keysToArray;

let valuesToArray = Belt_internalAVLtree.valuesToArray;

let minKey = Belt_internalAVLtree.minKey;

let minKeyUndefined = Belt_internalAVLtree.minKeyUndefined;

let maxKey = Belt_internalAVLtree.maxKey;

let maxKeyUndefined = Belt_internalAVLtree.maxKeyUndefined;

let minimum = Belt_internalAVLtree.minimum;

let minUndefined = Belt_internalAVLtree.minUndefined;

let maximum = Belt_internalAVLtree.maximum;

let maxUndefined = Belt_internalAVLtree.maxUndefined;

let get = Belt_internalMapString.get;

let getUndefined = Belt_internalMapString.getUndefined;

let getWithDefault = Belt_internalMapString.getWithDefault;

let getExn = Belt_internalMapString.getOrThrow;

let getOrThrow = Belt_internalMapString.getOrThrow;

let checkInvariantInternal = Belt_internalAVLtree.checkInvariantInternal;

let updateU = update;

let mergeU = Belt_internalMapString.merge;

let merge = Belt_internalMapString.merge;

let keepU = Belt_internalAVLtree.keepShared;

let keep = Belt_internalAVLtree.keepShared;

let partitionU = Belt_internalAVLtree.partitionShared;

let partition = Belt_internalAVLtree.partitionShared;

let split = Belt_internalMapString.split;

let mapU = Belt_internalAVLtree.map;

let map = Belt_internalAVLtree.map;

let mapWithKeyU = Belt_internalAVLtree.mapWithKey;

let mapWithKey = Belt_internalAVLtree.mapWithKey;

export {
  empty,
  isEmpty,
  has,
  cmpU,
  cmp,
  eqU,
  eq,
  findFirstByU,
  findFirstBy,
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
  mergeU,
  merge,
  mergeMany,
  keepU,
  keep,
  partitionU,
  partition,
  split,
  mapU,
  map,
  mapWithKeyU,
  mapWithKey,
}
/* No side effect */

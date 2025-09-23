'use strict';

let Primitive_option = require("./Primitive_option.js");
let Belt_internalMapInt = require("./Belt_internalMapInt.js");
let Belt_internalAVLtree = require("./Belt_internalAVLtree.js");

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

let has = Belt_internalMapInt.has;

let cmpU = Belt_internalMapInt.cmp;

let cmp = Belt_internalMapInt.cmp;

let eqU = Belt_internalMapInt.eq;

let eq = Belt_internalMapInt.eq;

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

let fromArray = Belt_internalMapInt.fromArray;

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

let get = Belt_internalMapInt.get;

let getUndefined = Belt_internalMapInt.getUndefined;

let getWithDefault = Belt_internalMapInt.getWithDefault;

let getExn = Belt_internalMapInt.getOrThrow;

let getOrThrow = Belt_internalMapInt.getOrThrow;

let checkInvariantInternal = Belt_internalAVLtree.checkInvariantInternal;

let updateU = update;

let mergeU = Belt_internalMapInt.merge;

let merge = Belt_internalMapInt.merge;

let keepU = Belt_internalAVLtree.keepShared;

let keep = Belt_internalAVLtree.keepShared;

let partitionU = Belt_internalAVLtree.partitionShared;

let partition = Belt_internalAVLtree.partitionShared;

let split = Belt_internalMapInt.split;

let mapU = Belt_internalAVLtree.map;

let map = Belt_internalAVLtree.map;

let mapWithKeyU = Belt_internalAVLtree.mapWithKey;

let mapWithKey = Belt_internalAVLtree.mapWithKey;

exports.empty = empty;
exports.isEmpty = isEmpty;
exports.has = has;
exports.cmpU = cmpU;
exports.cmp = cmp;
exports.eqU = eqU;
exports.eq = eq;
exports.findFirstByU = findFirstByU;
exports.findFirstBy = findFirstBy;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.everyU = everyU;
exports.every = every;
exports.someU = someU;
exports.some = some;
exports.size = size;
exports.toList = toList;
exports.toArray = toArray;
exports.fromArray = fromArray;
exports.keysToArray = keysToArray;
exports.valuesToArray = valuesToArray;
exports.minKey = minKey;
exports.minKeyUndefined = minKeyUndefined;
exports.maxKey = maxKey;
exports.maxKeyUndefined = maxKeyUndefined;
exports.minimum = minimum;
exports.minUndefined = minUndefined;
exports.maximum = maximum;
exports.maxUndefined = maxUndefined;
exports.get = get;
exports.getUndefined = getUndefined;
exports.getWithDefault = getWithDefault;
exports.getExn = getExn;
exports.getOrThrow = getOrThrow;
exports.checkInvariantInternal = checkInvariantInternal;
exports.remove = remove;
exports.removeMany = removeMany;
exports.set = set;
exports.updateU = updateU;
exports.update = update;
exports.mergeU = mergeU;
exports.merge = merge;
exports.mergeMany = mergeMany;
exports.keepU = keepU;
exports.keep = keep;
exports.partitionU = partitionU;
exports.partition = partition;
exports.split = split;
exports.mapU = mapU;
exports.map = map;
exports.mapWithKeyU = mapWithKeyU;
exports.mapWithKey = mapWithKey;
/* No side effect */

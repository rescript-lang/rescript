'use strict';

let Primitive_option = require("./Primitive_option.js");
let Belt_internalAVLtree = require("./Belt_internalAVLtree.js");

function set(t, newK, newD, cmp) {
  if (t === undefined) {
    return Belt_internalAVLtree.singleton(newK, newD);
  }
  let k = t.k;
  let c = cmp(newK, k);
  if (c === 0) {
    return Belt_internalAVLtree.updateValue(t, newD);
  }
  let l = t.l;
  let r = t.r;
  let v = t.v;
  if (c < 0) {
    return Belt_internalAVLtree.bal(set(l, newK, newD, cmp), k, v, r);
  } else {
    return Belt_internalAVLtree.bal(l, k, v, set(r, newK, newD, cmp));
  }
}

function update(t, newK, f, cmp) {
  if (t !== undefined) {
    let k = t.k;
    let c = cmp(newK, k);
    if (c === 0) {
      let newD = f(Primitive_option.some(t.v));
      if (newD !== undefined) {
        return Belt_internalAVLtree.updateValue(t, Primitive_option.valFromOption(newD));
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
    let l$1 = t.l;
    let r$2 = t.r;
    let v = t.v;
    if (c < 0) {
      let ll = update(l$1, newK, f, cmp);
      if (l$1 === ll) {
        return t;
      } else {
        return Belt_internalAVLtree.bal(ll, k, v, r$2);
      }
    }
    let rr = update(r$2, newK, f, cmp);
    if (r$2 === rr) {
      return t;
    } else {
      return Belt_internalAVLtree.bal(l$1, k, v, rr);
    }
  }
  let newD$1 = f(undefined);
  if (newD$1 !== undefined) {
    return Belt_internalAVLtree.singleton(newK, Primitive_option.valFromOption(newD$1));
  } else {
    return t;
  }
}

function removeAux0(n, x, cmp) {
  let v = n.k;
  let l = n.l;
  let r = n.r;
  let c = cmp(x, v);
  if (c === 0) {
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
  if (c < 0) {
    if (l === undefined) {
      return n;
    }
    let ll = removeAux0(l, x, cmp);
    if (ll === l) {
      return n;
    } else {
      return Belt_internalAVLtree.bal(ll, v, n.v, r);
    }
  }
  if (r === undefined) {
    return n;
  }
  let rr = removeAux0(r, x, cmp);
  if (rr === r) {
    return n;
  } else {
    return Belt_internalAVLtree.bal(l, v, n.v, rr);
  }
}

function remove(n, x, cmp) {
  if (n !== undefined) {
    return removeAux0(n, x, cmp);
  }
}

function mergeMany(h, arr, cmp) {
  let len = arr.length;
  let v = h;
  for (let i = 0; i < len; ++i) {
    let match = arr[i];
    v = set(v, match[0], match[1], cmp);
  }
  return v;
}

function splitAuxPivot(n, x, pres, cmp) {
  let v = n.k;
  let d = n.v;
  let l = n.l;
  let r = n.r;
  let c = cmp(x, v);
  if (c === 0) {
    pres.contents = Primitive_option.some(d);
    return [
      l,
      r
    ];
  }
  if (c < 0) {
    if (l === undefined) {
      return [
        undefined,
        n
      ];
    }
    let match = splitAuxPivot(l, x, pres, cmp);
    return [
      match[0],
      Belt_internalAVLtree.join(match[1], v, d, r)
    ];
  }
  if (r === undefined) {
    return [
      n,
      undefined
    ];
  }
  let match$1 = splitAuxPivot(r, x, pres, cmp);
  return [
    Belt_internalAVLtree.join(l, v, d, match$1[0]),
    match$1[1]
  ];
}

function split(n, x, cmp) {
  if (n === undefined) {
    return [
      [
        undefined,
        undefined
      ],
      undefined
    ];
  }
  let pres = {
    contents: undefined
  };
  let v = splitAuxPivot(n, x, pres, cmp);
  return [
    v,
    pres.contents
  ];
}

function merge(s1, s2, f, cmp) {
  if (s1 === undefined) {
    if (s2 !== undefined) {
      return Belt_internalAVLtree.keepMap(s2, (k, v) => f(k, undefined, Primitive_option.some(v)));
    } else {
      return;
    }
  }
  if (s2 === undefined) {
    return Belt_internalAVLtree.keepMap(s1, (k, v) => f(k, Primitive_option.some(v), undefined));
  }
  if (s1.h >= s2.h) {
    let v1 = s1.k;
    let d1 = s1.v;
    let l1 = s1.l;
    let r1 = s1.r;
    let d2 = {
      contents: undefined
    };
    let match = splitAuxPivot(s2, v1, d2, cmp);
    let d2$1 = d2.contents;
    let newLeft = merge(l1, match[0], f, cmp);
    let newD = f(v1, Primitive_option.some(d1), d2$1);
    let newRight = merge(r1, match[1], f, cmp);
    return Belt_internalAVLtree.concatOrJoin(newLeft, v1, newD, newRight);
  }
  let v2 = s2.k;
  let d2$2 = s2.v;
  let l2 = s2.l;
  let r2 = s2.r;
  let d1$1 = {
    contents: undefined
  };
  let match$1 = splitAuxPivot(s1, v2, d1$1, cmp);
  let d1$2 = d1$1.contents;
  let newLeft$1 = merge(match$1[0], l2, f, cmp);
  let newD$1 = f(v2, d1$2, Primitive_option.some(d2$2));
  let newRight$1 = merge(match$1[1], r2, f, cmp);
  return Belt_internalAVLtree.concatOrJoin(newLeft$1, v2, newD$1, newRight$1);
}

function removeMany(t, keys, cmp) {
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
      let u = removeAux0(t$1, ele, cmp);
      if (u === undefined) {
        return u;
      }
      _i = i + 1 | 0;
      _t = u;
      continue;
    };
  }
}

let empty;

let isEmpty = Belt_internalAVLtree.isEmpty;

let has = Belt_internalAVLtree.has;

let cmpU = Belt_internalAVLtree.cmp;

let cmp = Belt_internalAVLtree.cmp;

let eqU = Belt_internalAVLtree.eq;

let eq = Belt_internalAVLtree.eq;

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

let fromArray = Belt_internalAVLtree.fromArray;

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

let get = Belt_internalAVLtree.get;

let getUndefined = Belt_internalAVLtree.getUndefined;

let getWithDefault = Belt_internalAVLtree.getWithDefault;

let getExn = Belt_internalAVLtree.getOrThrow;

let getOrThrow = Belt_internalAVLtree.getOrThrow;

let checkInvariantInternal = Belt_internalAVLtree.checkInvariantInternal;

let updateU = update;

let mergeU = merge;

let keepU = Belt_internalAVLtree.keepShared;

let keep = Belt_internalAVLtree.keepShared;

let partitionU = Belt_internalAVLtree.partitionShared;

let partition = Belt_internalAVLtree.partitionShared;

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

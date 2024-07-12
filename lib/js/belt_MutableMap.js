'use strict';

let Caml_option = require("./caml_option.js");
let Belt_internalAVLtree = require("./belt_internalAVLtree.js");

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
  while(true) {
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
      let data = f(Caml_option.some(t.v));
      if (data !== undefined) {
        t.v = Caml_option.valFromOption(data);
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
    return Belt_internalAVLtree.singleton(x, Caml_option.valFromOption(data$1));
  } else {
    return t;
  }
}

function updateU(t, x, f) {
  let oldRoot = t.data;
  let newRoot = updateDone(oldRoot, x, f, t.cmp);
  if (newRoot !== oldRoot) {
    t.data = newRoot;
    return;
  }
  
}

function update(t, x, f) {
  updateU(t, x, (function (a) {
    return f(a);
  }));
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
  return d.data === undefined;
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

function forEachU(d, f) {
  Belt_internalAVLtree.forEachU(d.data, f);
}

function forEach(d, f) {
  forEachU(d, (function (a, b) {
    f(a, b);
  }));
}

function reduceU(d, acc, cb) {
  return Belt_internalAVLtree.reduceU(d.data, acc, cb);
}

function reduce(d, acc, cb) {
  return reduceU(d, acc, (function (a, b, c) {
    return cb(a, b, c);
  }));
}

function everyU(d, p) {
  return Belt_internalAVLtree.everyU(d.data, p);
}

function every(d, p) {
  return everyU(d, (function (a, b) {
    return p(a, b);
  }));
}

function someU(d, p) {
  return Belt_internalAVLtree.someU(d.data, p);
}

function some(d, p) {
  return someU(d, (function (a, b) {
    return p(a, b);
  }));
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

function cmpU(m1, m2, cmp) {
  return Belt_internalAVLtree.cmpU(m1.data, m2.data, m1.cmp, cmp);
}

function cmp(m1, m2, cmp$1) {
  return cmpU(m1, m2, (function (a, b) {
    return cmp$1(a, b);
  }));
}

function eqU(m1, m2, cmp) {
  return Belt_internalAVLtree.eqU(m1.data, m2.data, m1.cmp, cmp);
}

function eq(m1, m2, cmp) {
  return eqU(m1, m2, (function (a, b) {
    return cmp(a, b);
  }));
}

function mapU(m, f) {
  return {
    cmp: m.cmp,
    data: Belt_internalAVLtree.mapU(m.data, f)
  };
}

function map(m, f) {
  return mapU(m, (function (a) {
    return f(a);
  }));
}

function mapWithKeyU(m, f) {
  return {
    cmp: m.cmp,
    data: Belt_internalAVLtree.mapWithKeyU(m.data, f)
  };
}

function mapWithKey(m, f) {
  return mapWithKeyU(m, (function (a, b) {
    return f(a, b);
  }));
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

function getExn(m, x) {
  return Belt_internalAVLtree.getExn(m.data, x, m.cmp);
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
  for(let i = 0 ,i_finish = xs.length; i < i_finish; ++i){
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

exports.Int = Int;
exports.$$String = $$String;
exports.make = make;
exports.clear = clear;
exports.isEmpty = isEmpty;
exports.has = has;
exports.cmpU = cmpU;
exports.cmp = cmp;
exports.eqU = eqU;
exports.eq = eq;
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
exports.checkInvariantInternal = checkInvariantInternal;
exports.remove = remove;
exports.removeMany = removeMany;
exports.set = set;
exports.updateU = updateU;
exports.update = update;
exports.mergeMany = mergeMany;
exports.mapU = mapU;
exports.map = map;
exports.mapWithKeyU = mapWithKeyU;
exports.mapWithKey = mapWithKey;
/* No side effect */

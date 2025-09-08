

import * as Belt_SetDict from "./Belt_SetDict.js";

function fromArray(data, id) {
  let cmp = id.cmp;
  return {
    cmp: cmp,
    data: Belt_SetDict.fromArray(data, cmp)
  };
}

function remove(m, e) {
  let data = m.data;
  let cmp = m.cmp;
  let newData = Belt_SetDict.remove(data, e, cmp);
  if (newData === data) {
    return m;
  } else {
    return {
      cmp: cmp,
      data: newData
    };
  }
}

function add(m, e) {
  let data = m.data;
  let cmp = m.cmp;
  let newData = Belt_SetDict.add(data, e, cmp);
  if (newData === data) {
    return m;
  } else {
    return {
      cmp: cmp,
      data: newData
    };
  }
}

function mergeMany(m, e) {
  let cmp = m.cmp;
  return {
    cmp: cmp,
    data: Belt_SetDict.mergeMany(m.data, e, cmp)
  };
}

function removeMany(m, e) {
  let cmp = m.cmp;
  return {
    cmp: cmp,
    data: Belt_SetDict.removeMany(m.data, e, cmp)
  };
}

function union(m, n) {
  let cmp = m.cmp;
  return {
    cmp: cmp,
    data: Belt_SetDict.union(m.data, n.data, cmp)
  };
}

function intersect(m, n) {
  let cmp = m.cmp;
  return {
    cmp: cmp,
    data: Belt_SetDict.intersect(m.data, n.data, cmp)
  };
}

function diff(m, n) {
  let cmp = m.cmp;
  return {
    cmp: cmp,
    data: Belt_SetDict.diff(m.data, n.data, cmp)
  };
}

function subset(m, n) {
  let cmp = m.cmp;
  return Belt_SetDict.subset(m.data, n.data, cmp);
}

function split(m, e) {
  let cmp = m.cmp;
  let match = Belt_SetDict.split(m.data, e, cmp);
  let match$1 = match[0];
  return [
    [
      {
        cmp: cmp,
        data: match$1[0]
      },
      {
        cmp: cmp,
        data: match$1[1]
      }
    ],
    match[1]
  ];
}

function make(id) {
  return {
    cmp: id.cmp,
    data: undefined
  };
}

function isEmpty(m) {
  return Belt_SetDict.isEmpty(m.data);
}

function cmp(m, n) {
  let cmp$1 = m.cmp;
  return Belt_SetDict.cmp(m.data, n.data, cmp$1);
}

function eq(m, n) {
  return Belt_SetDict.eq(m.data, n.data, m.cmp);
}

function forEach(m, f) {
  Belt_SetDict.forEach(m.data, f);
}

function reduce(m, acc, f) {
  return Belt_SetDict.reduce(m.data, acc, f);
}

function every(m, f) {
  return Belt_SetDict.every(m.data, f);
}

function some(m, f) {
  return Belt_SetDict.some(m.data, f);
}

function keep(m, f) {
  return {
    cmp: m.cmp,
    data: Belt_SetDict.keep(m.data, f)
  };
}

function partition(m, f) {
  let match = Belt_SetDict.partition(m.data, f);
  let cmp = m.cmp;
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

function size(m) {
  return Belt_SetDict.size(m.data);
}

function toList(m) {
  return Belt_SetDict.toList(m.data);
}

function toArray(m) {
  return Belt_SetDict.toArray(m.data);
}

function minimum(m) {
  return Belt_SetDict.minimum(m.data);
}

function minUndefined(m) {
  return Belt_SetDict.minUndefined(m.data);
}

function maximum(m) {
  return Belt_SetDict.maximum(m.data);
}

function maxUndefined(m) {
  return Belt_SetDict.maxUndefined(m.data);
}

function get(m, e) {
  return Belt_SetDict.get(m.data, e, m.cmp);
}

function getUndefined(m, e) {
  return Belt_SetDict.getUndefined(m.data, e, m.cmp);
}

function getOrThrow(m, e) {
  return Belt_SetDict.getOrThrow(m.data, e, m.cmp);
}

function has(m, e) {
  return Belt_SetDict.has(m.data, e, m.cmp);
}

function fromSortedArrayUnsafe(xs, id) {
  return {
    cmp: id.cmp,
    data: Belt_SetDict.fromSortedArrayUnsafe(xs)
  };
}

function getData(m) {
  return m.data;
}

function getId(m) {
  let cmp = m.cmp;
  return {
    cmp: cmp
  };
}

function packIdData(id, data) {
  return {
    cmp: id.cmp,
    data: data
  };
}

function checkInvariantInternal(d) {
  Belt_SetDict.checkInvariantInternal(d.data);
}

let Int;

let $$String;

let Dict;

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
  Dict,
  make,
  fromArray,
  fromSortedArrayUnsafe,
  isEmpty,
  has,
  add,
  mergeMany,
  remove,
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
  toArray,
  toList,
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
  getData,
  getId,
  packIdData,
}
/* No side effect */

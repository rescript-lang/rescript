'use strict';

let Belt_MapDict = require("./Belt_MapDict.js");

function fromArray(data, id) {
  let cmp = id.cmp;
  return {
    cmp: cmp,
    data: Belt_MapDict.fromArray(data, cmp)
  };
}

function remove(m, x) {
  let odata = m.data;
  let cmp = m.cmp;
  let newData = Belt_MapDict.remove(odata, x, cmp);
  if (newData === odata) {
    return m;
  } else {
    return {
      cmp: cmp,
      data: newData
    };
  }
}

function removeMany(m, x) {
  let cmp = m.cmp;
  let newData = Belt_MapDict.removeMany(m.data, x, cmp);
  return {
    cmp: cmp,
    data: newData
  };
}

function set(m, key, d) {
  let cmp = m.cmp;
  return {
    cmp: cmp,
    data: Belt_MapDict.set(m.data, key, d, cmp)
  };
}

function mergeMany(m, e) {
  let cmp = m.cmp;
  return {
    cmp: cmp,
    data: Belt_MapDict.mergeMany(m.data, e, cmp)
  };
}

function update(m, key, f) {
  let cmp = m.cmp;
  return {
    cmp: cmp,
    data: Belt_MapDict.update(m.data, key, f, cmp)
  };
}

function split(m, x) {
  let cmp = m.cmp;
  let match = Belt_MapDict.split(m.data, x, cmp);
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

function merge(s1, s2, f) {
  let cmp = s1.cmp;
  return {
    cmp: cmp,
    data: Belt_MapDict.merge(s1.data, s2.data, f, cmp)
  };
}

function make(id) {
  return {
    cmp: id.cmp,
    data: undefined
  };
}

function isEmpty(map) {
  return Belt_MapDict.isEmpty(map.data);
}

function findFirstBy(m, f) {
  return Belt_MapDict.findFirstBy(m.data, f);
}

function forEach(m, f) {
  Belt_MapDict.forEach(m.data, f);
}

function reduce(m, acc, f) {
  return Belt_MapDict.reduce(m.data, acc, f);
}

function every(m, f) {
  return Belt_MapDict.every(m.data, f);
}

function some(m, f) {
  return Belt_MapDict.some(m.data, f);
}

function keep(m, f) {
  return {
    cmp: m.cmp,
    data: Belt_MapDict.keep(m.data, f)
  };
}

function partition(m, p) {
  let cmp = m.cmp;
  let match = Belt_MapDict.partition(m.data, p);
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

function map(m, f) {
  return {
    cmp: m.cmp,
    data: Belt_MapDict.map(m.data, f)
  };
}

function mapWithKey(m, f) {
  return {
    cmp: m.cmp,
    data: Belt_MapDict.mapWithKey(m.data, f)
  };
}

function size(map) {
  return Belt_MapDict.size(map.data);
}

function toList(map) {
  return Belt_MapDict.toList(map.data);
}

function toArray(m) {
  return Belt_MapDict.toArray(m.data);
}

function keysToArray(m) {
  return Belt_MapDict.keysToArray(m.data);
}

function valuesToArray(m) {
  return Belt_MapDict.valuesToArray(m.data);
}

function minKey(m) {
  return Belt_MapDict.minKey(m.data);
}

function minKeyUndefined(m) {
  return Belt_MapDict.minKeyUndefined(m.data);
}

function maxKey(m) {
  return Belt_MapDict.maxKey(m.data);
}

function maxKeyUndefined(m) {
  return Belt_MapDict.maxKeyUndefined(m.data);
}

function minimum(m) {
  return Belt_MapDict.minimum(m.data);
}

function minUndefined(m) {
  return Belt_MapDict.minUndefined(m.data);
}

function maximum(m) {
  return Belt_MapDict.maximum(m.data);
}

function maxUndefined(m) {
  return Belt_MapDict.maxUndefined(m.data);
}

function get(map, x) {
  return Belt_MapDict.get(map.data, x, map.cmp);
}

function getUndefined(map, x) {
  return Belt_MapDict.getUndefined(map.data, x, map.cmp);
}

function getWithDefault(map, x, def) {
  return Belt_MapDict.getWithDefault(map.data, x, def, map.cmp);
}

function getOrThrow(map, x) {
  return Belt_MapDict.getOrThrow(map.data, x, map.cmp);
}

function has(map, x) {
  return Belt_MapDict.has(map.data, x, map.cmp);
}

function checkInvariantInternal(m) {
  Belt_MapDict.checkInvariantInternal(m.data);
}

function eq(m1, m2, veq) {
  return Belt_MapDict.eq(m1.data, m2.data, m1.cmp, veq);
}

function cmp(m1, m2, vcmp) {
  return Belt_MapDict.cmp(m1.data, m2.data, m1.cmp, vcmp);
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

let Int;

let $$String;

let Dict;

let cmpU = cmp;

let eqU = eq;

let findFirstByU = findFirstBy;

let forEachU = forEach;

let reduceU = reduce;

let everyU = every;

let someU = some;

let getExn = getOrThrow;

let updateU = update;

let mergeU = merge;

let keepU = keep;

let partitionU = partition;

let mapU = map;

let mapWithKeyU = mapWithKey;

exports.Int = Int;
exports.$$String = $$String;
exports.Dict = Dict;
exports.make = make;
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
exports.toArray = toArray;
exports.toList = toList;
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
exports.remove = remove;
exports.removeMany = removeMany;
exports.set = set;
exports.updateU = updateU;
exports.update = update;
exports.mergeMany = mergeMany;
exports.mergeU = mergeU;
exports.merge = merge;
exports.keepU = keepU;
exports.keep = keep;
exports.partitionU = partitionU;
exports.partition = partition;
exports.split = split;
exports.mapU = mapU;
exports.map = map;
exports.mapWithKeyU = mapWithKeyU;
exports.mapWithKey = mapWithKey;
exports.getData = getData;
exports.getId = getId;
exports.packIdData = packIdData;
exports.checkInvariantInternal = checkInvariantInternal;
/* No side effect */

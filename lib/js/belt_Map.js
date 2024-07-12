'use strict';

let Belt_MapDict = require("./belt_MapDict.js");

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

function updateU(m, key, f) {
  let cmp = m.cmp;
  return {
    cmp: cmp,
    data: Belt_MapDict.updateU(m.data, key, f, cmp)
  };
}

function update(m, key, f) {
  return updateU(m, key, (function (a) {
    return f(a);
  }));
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

function mergeU(s1, s2, f) {
  let cmp = s1.cmp;
  return {
    cmp: cmp,
    data: Belt_MapDict.mergeU(s1.data, s2.data, f, cmp)
  };
}

function merge(s1, s2, f) {
  return mergeU(s1, s2, (function (a, b, c) {
    return f(a, b, c);
  }));
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

function findFirstByU(m, f) {
  return Belt_MapDict.findFirstByU(m.data, f);
}

function findFirstBy(m, f) {
  return findFirstByU(m, (function (a, b) {
    return f(a, b);
  }));
}

function forEachU(m, f) {
  Belt_MapDict.forEachU(m.data, f);
}

function forEach(m, f) {
  forEachU(m, (function (a, b) {
    f(a, b);
  }));
}

function reduceU(m, acc, f) {
  return Belt_MapDict.reduceU(m.data, acc, f);
}

function reduce(m, acc, f) {
  return reduceU(m, acc, (function (a, b, c) {
    return f(a, b, c);
  }));
}

function everyU(m, f) {
  return Belt_MapDict.everyU(m.data, f);
}

function every(m, f) {
  return everyU(m, (function (a, b) {
    return f(a, b);
  }));
}

function someU(m, f) {
  return Belt_MapDict.someU(m.data, f);
}

function some(m, f) {
  return someU(m, (function (a, b) {
    return f(a, b);
  }));
}

function keepU(m, f) {
  return {
    cmp: m.cmp,
    data: Belt_MapDict.keepU(m.data, f)
  };
}

function keep(m, f) {
  return keepU(m, (function (a, b) {
    return f(a, b);
  }));
}

function partitionU(m, p) {
  let cmp = m.cmp;
  let match = Belt_MapDict.partitionU(m.data, p);
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

function partition(m, p) {
  return partitionU(m, (function (a, b) {
    return p(a, b);
  }));
}

function mapU(m, f) {
  return {
    cmp: m.cmp,
    data: Belt_MapDict.mapU(m.data, f)
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
    data: Belt_MapDict.mapWithKeyU(m.data, f)
  };
}

function mapWithKey(m, f) {
  return mapWithKeyU(m, (function (a, b) {
    return f(a, b);
  }));
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

function getExn(map, x) {
  return Belt_MapDict.getExn(map.data, x, map.cmp);
}

function has(map, x) {
  return Belt_MapDict.has(map.data, x, map.cmp);
}

function checkInvariantInternal(m) {
  Belt_MapDict.checkInvariantInternal(m.data);
}

function eqU(m1, m2, veq) {
  return Belt_MapDict.eqU(m1.data, m2.data, m1.cmp, veq);
}

function eq(m1, m2, veq) {
  return eqU(m1, m2, (function (a, b) {
    return veq(a, b);
  }));
}

function cmpU(m1, m2, vcmp) {
  return Belt_MapDict.cmpU(m1.data, m2.data, m1.cmp, vcmp);
}

function cmp(m1, m2, vcmp) {
  return cmpU(m1, m2, (function (a, b) {
    return vcmp(a, b);
  }));
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

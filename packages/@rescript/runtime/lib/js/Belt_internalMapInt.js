'use strict';

let Primitive_int = require("./Primitive_int.js");
let Belt_SortArray = require("./Belt_SortArray.js");
let Primitive_option = require("./Primitive_option.js");
let Belt_internalAVLtree = require("./Belt_internalAVLtree.js");

function add(t, x, data) {
  if (t === undefined) {
    return Belt_internalAVLtree.singleton(x, data);
  }
  let k = t.k;
  if (x === k) {
    return Belt_internalAVLtree.updateValue(t, data);
  }
  let v = t.v;
  if (x < k) {
    return Belt_internalAVLtree.bal(add(t.l, x, data), k, v, t.r);
  } else {
    return Belt_internalAVLtree.bal(t.l, k, v, add(t.r, x, data));
  }
}

function get(_n, x) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return;
    }
    let v = n.k;
    if (x === v) {
      return Primitive_option.some(n.v);
    }
    _n = x < v ? n.l : n.r;
    continue;
  };
}

function getUndefined(_n, x) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return;
    }
    let v = n.k;
    if (x === v) {
      return n.v;
    }
    _n = x < v ? n.l : n.r;
    continue;
  };
}

function getOrThrow(_n, x) {
  while (true) {
    let n = _n;
    if (n !== undefined) {
      let v = n.k;
      if (x === v) {
        return n.v;
      }
      _n = x < v ? n.l : n.r;
      continue;
    }
    throw {
      RE_EXN_ID: "Not_found",
      Error: new Error()
    };
  };
}

function getWithDefault(_n, x, def) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return def;
    }
    let v = n.k;
    if (x === v) {
      return n.v;
    }
    _n = x < v ? n.l : n.r;
    continue;
  };
}

function has(_n, x) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return false;
    }
    let v = n.k;
    if (x === v) {
      return true;
    }
    _n = x < v ? n.l : n.r;
    continue;
  };
}

function remove(n, x) {
  if (n === undefined) {
    return n;
  }
  let v = n.k;
  let l = n.l;
  let r = n.r;
  if (x !== v) {
    if (x < v) {
      return Belt_internalAVLtree.bal(remove(l, x), v, n.v, r);
    } else {
      return Belt_internalAVLtree.bal(l, v, n.v, remove(r, x));
    }
  }
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

function splitAux(x, n) {
  let v = n.k;
  let d = n.v;
  let l = n.l;
  let r = n.r;
  if (x === v) {
    return [
      l,
      Primitive_option.some(d),
      r
    ];
  }
  if (x < v) {
    if (l === undefined) {
      return [
        undefined,
        undefined,
        n
      ];
    }
    let match = splitAux(x, l);
    return [
      match[0],
      match[1],
      Belt_internalAVLtree.join(match[2], v, d, r)
    ];
  }
  if (r === undefined) {
    return [
      n,
      undefined,
      undefined
    ];
  }
  let match$1 = splitAux(x, r);
  return [
    Belt_internalAVLtree.join(l, v, d, match$1[0]),
    match$1[1],
    match$1[2]
  ];
}

function split(x, n) {
  if (n !== undefined) {
    return splitAux(x, n);
  } else {
    return [
      undefined,
      undefined,
      undefined
    ];
  }
}

function merge(s1, s2, f) {
  if (s1 !== undefined) {
    if (s1.h >= (
        s2 !== undefined ? s2.h : 0
      )) {
      let v1 = s1.k;
      let d1 = s1.v;
      let l1 = s1.l;
      let r1 = s1.r;
      let match = split(v1, s2);
      return Belt_internalAVLtree.concatOrJoin(merge(l1, match[0], f), v1, f(v1, Primitive_option.some(d1), match[1]), merge(r1, match[2], f));
    }
  } else if (s2 === undefined) {
    return;
  }
  let v2 = s2.k;
  let d2 = s2.v;
  let l2 = s2.l;
  let r2 = s2.r;
  let match$1 = split(v2, s1);
  return Belt_internalAVLtree.concatOrJoin(merge(match$1[0], l2, f), v2, f(v2, match$1[1], Primitive_option.some(d2)), merge(match$1[2], r2, f));
}

function compareAux(_e1, _e2, vcmp) {
  while (true) {
    let e2 = _e2;
    let e1 = _e1;
    if (e1 === 0) {
      return 0;
    }
    if (e2 === 0) {
      return 0;
    }
    let h2 = e2.hd;
    let h1 = e1.hd;
    let c = Primitive_int.compare(h1.k, h2.k);
    if (c !== 0) {
      return c;
    }
    let cx = vcmp(h1.v, h2.v);
    if (cx !== 0) {
      return cx;
    }
    _e2 = Belt_internalAVLtree.stackAllLeft(h2.r, e2.tl);
    _e1 = Belt_internalAVLtree.stackAllLeft(h1.r, e1.tl);
    continue;
  };
}

function cmp(s1, s2, cmp$1) {
  let len1 = Belt_internalAVLtree.size(s1);
  let len2 = Belt_internalAVLtree.size(s2);
  if (len1 === len2) {
    return compareAux(Belt_internalAVLtree.stackAllLeft(s1, /* [] */0), Belt_internalAVLtree.stackAllLeft(s2, /* [] */0), cmp$1);
  } else if (len1 < len2) {
    return -1;
  } else {
    return 1;
  }
}

function eqAux(_e1, _e2, eq) {
  while (true) {
    let e2 = _e2;
    let e1 = _e1;
    if (e1 === 0) {
      return true;
    }
    if (e2 === 0) {
      return true;
    }
    let h2 = e2.hd;
    let h1 = e1.hd;
    if (!(h1.k === h2.k && eq(h1.v, h2.v))) {
      return false;
    }
    _e2 = Belt_internalAVLtree.stackAllLeft(h2.r, e2.tl);
    _e1 = Belt_internalAVLtree.stackAllLeft(h1.r, e1.tl);
    continue;
  };
}

function eq(s1, s2, eq$1) {
  let len1 = Belt_internalAVLtree.size(s1);
  let len2 = Belt_internalAVLtree.size(s2);
  if (len1 === len2) {
    return eqAux(Belt_internalAVLtree.stackAllLeft(s1, /* [] */0), Belt_internalAVLtree.stackAllLeft(s2, /* [] */0), eq$1);
  } else {
    return false;
  }
}

function addMutate(t, x, data) {
  if (t === undefined) {
    return Belt_internalAVLtree.singleton(x, data);
  }
  let k = t.k;
  if (x === k) {
    t.k = x;
    t.v = data;
    return t;
  }
  let l = t.l;
  let r = t.r;
  if (x < k) {
    let ll = addMutate(l, x, data);
    t.l = ll;
  } else {
    t.r = addMutate(r, x, data);
  }
  return Belt_internalAVLtree.balMutate(t);
}

function fromArray(xs) {
  let len = xs.length;
  if (len === 0) {
    return;
  }
  let next = Belt_SortArray.strictlySortedLength(xs, (param, param$1) => param[0] < param$1[0]);
  let result;
  if (next >= 0) {
    result = Belt_internalAVLtree.fromSortedArrayAux(xs, 0, next);
  } else {
    next = -next | 0;
    result = Belt_internalAVLtree.fromSortedArrayRevAux(xs, next - 1 | 0, next);
  }
  for (let i = next; i < len; ++i) {
    let match = xs[i];
    result = addMutate(result, match[0], match[1]);
  }
  return result;
}

let N;

let A;

let S;

let cmpU = cmp;

let eqU = eq;

let mergeU = merge;

exports.N = N;
exports.A = A;
exports.S = S;
exports.add = add;
exports.get = get;
exports.getUndefined = getUndefined;
exports.getOrThrow = getOrThrow;
exports.getWithDefault = getWithDefault;
exports.has = has;
exports.remove = remove;
exports.splitAux = splitAux;
exports.split = split;
exports.merge = merge;
exports.compareAux = compareAux;
exports.cmp = cmp;
exports.eqAux = eqAux;
exports.eq = eq;
exports.addMutate = addMutate;
exports.fromArray = fromArray;
exports.cmpU = cmpU;
exports.eqU = eqU;
exports.mergeU = mergeU;
/* No side effect */

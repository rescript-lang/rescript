

import * as Belt_internalAVLset from "./Belt_internalAVLset.js";
import * as Belt_SortArrayString from "./Belt_SortArrayString.js";

function has(_t, x) {
  while (true) {
    let t = _t;
    if (t === undefined) {
      return false;
    }
    let v = t.v;
    if (x === v) {
      return true;
    }
    _t = x < v ? t.l : t.r;
    continue;
  };
}

function compareAux(_e1, _e2) {
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
    let k1 = h1.v;
    let k2 = h2.v;
    if (k1 !== k2) {
      if (k1 < k2) {
        return -1;
      } else {
        return 1;
      }
    }
    _e2 = Belt_internalAVLset.stackAllLeft(h2.r, e2.tl);
    _e1 = Belt_internalAVLset.stackAllLeft(h1.r, e1.tl);
    continue;
  };
}

function cmp(s1, s2) {
  let len1 = Belt_internalAVLset.size(s1);
  let len2 = Belt_internalAVLset.size(s2);
  if (len1 === len2) {
    return compareAux(Belt_internalAVLset.stackAllLeft(s1, /* [] */0), Belt_internalAVLset.stackAllLeft(s2, /* [] */0));
  } else if (len1 < len2) {
    return -1;
  } else {
    return 1;
  }
}

function eq(s1, s2) {
  return cmp(s1, s2) === 0;
}

function subset(_s1, _s2) {
  while (true) {
    let s2 = _s2;
    let s1 = _s1;
    if (s1 === undefined) {
      return true;
    }
    if (s2 === undefined) {
      return false;
    }
    let v1 = s1.v;
    let l1 = s1.l;
    let r1 = s1.r;
    let v2 = s2.v;
    let l2 = s2.l;
    let r2 = s2.r;
    if (v1 === v2) {
      if (!subset(l1, l2)) {
        return false;
      }
      _s2 = r2;
      _s1 = r1;
      continue;
    }
    if (v1 < v2) {
      if (!subset(Belt_internalAVLset.create(l1, v1, undefined), l2)) {
        return false;
      }
      _s1 = r1;
      continue;
    }
    if (!subset(Belt_internalAVLset.create(undefined, v1, r1), r2)) {
      return false;
    }
    _s1 = l1;
    continue;
  };
}

function get(_n, x) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return;
    }
    let v = n.v;
    if (x === v) {
      return v;
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
    let v = n.v;
    if (x === v) {
      return v;
    }
    _n = x < v ? n.l : n.r;
    continue;
  };
}

function getOrThrow(_n, x) {
  while (true) {
    let n = _n;
    if (n !== undefined) {
      let v = n.v;
      if (x === v) {
        return v;
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

function addMutate(t, x) {
  if (t === undefined) {
    return Belt_internalAVLset.singleton(x);
  }
  let k = t.v;
  if (x === k) {
    return t;
  }
  let l = t.l;
  let r = t.r;
  if (x < k) {
    t.l = addMutate(l, x);
  } else {
    t.r = addMutate(r, x);
  }
  return Belt_internalAVLset.balMutate(t);
}

function fromArray(xs) {
  let len = xs.length;
  if (len === 0) {
    return;
  }
  let next = Belt_SortArrayString.strictlySortedLength(xs);
  let result;
  if (next >= 0) {
    result = Belt_internalAVLset.fromSortedArrayAux(xs, 0, next);
  } else {
    next = -next | 0;
    result = Belt_internalAVLset.fromSortedArrayRevAux(xs, next - 1 | 0, next);
  }
  for (let i = next; i < len; ++i) {
    result = addMutate(result, xs[i]);
  }
  return result;
}

let S;

let N;

let A;

export {
  S,
  N,
  A,
  has,
  compareAux,
  cmp,
  eq,
  subset,
  get,
  getUndefined,
  getOrThrow,
  addMutate,
  fromArray,
}
/* No side effect */

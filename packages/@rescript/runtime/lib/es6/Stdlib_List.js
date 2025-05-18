

import * as Stdlib_Array from "./Stdlib_Array.js";
import * as Primitive_int from "./Primitive_int.js";
import * as Primitive_option from "./Primitive_option.js";

function head(x) {
  if (x !== 0) {
    return Primitive_option.some(x.hd);
  }
  
}

function headExn(x) {
  if (x !== 0) {
    return x.hd;
  }
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}

function tail(x) {
  if (x !== 0) {
    return x.tl;
  }
  
}

function tailExn(x) {
  if (x !== 0) {
    return x.tl;
  }
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}

function add(xs, x) {
  return {
    hd: x,
    tl: xs
  };
}

function get(x, n) {
  if (n < 0) {
    return;
  } else {
    let _x = x;
    let _n = n;
    while (true) {
      let n$1 = _n;
      let x$1 = _x;
      if (x$1 === 0) {
        return;
      }
      if (n$1 === 0) {
        return Primitive_option.some(x$1.hd);
      }
      _n = n$1 - 1 | 0;
      _x = x$1.tl;
      continue;
    };
  }
}

function getExn(x, n) {
  if (n < 0) {
    throw {
      RE_EXN_ID: "Not_found",
      Error: new Error()
    };
  }
  let _x = x;
  let _n = n;
  while (true) {
    let n$1 = _n;
    let x$1 = _x;
    if (x$1 !== 0) {
      if (n$1 === 0) {
        return x$1.hd;
      }
      _n = n$1 - 1 | 0;
      _x = x$1.tl;
      continue;
    }
    throw {
      RE_EXN_ID: "Not_found",
      Error: new Error()
    };
  };
}

function partitionAux(p, _cell, _precX, _precY) {
  while (true) {
    let precY = _precY;
    let precX = _precX;
    let cell = _cell;
    if (cell === 0) {
      return;
    }
    let t = cell.tl;
    let h = cell.hd;
    let next = {
      hd: h,
      tl: /* [] */0
    };
    if (p(h)) {
      precX.tl = next;
      _precX = next;
      _cell = t;
      continue;
    }
    precY.tl = next;
    _precY = next;
    _cell = t;
    continue;
  };
}

function splitAux(_cell, _precX, _precY) {
  while (true) {
    let precY = _precY;
    let precX = _precX;
    let cell = _cell;
    if (cell === 0) {
      return;
    }
    let match = cell.hd;
    let nextA = {
      hd: match[0],
      tl: /* [] */0
    };
    let nextB = {
      hd: match[1],
      tl: /* [] */0
    };
    precX.tl = nextA;
    precY.tl = nextB;
    _precY = nextB;
    _precX = nextA;
    _cell = cell.tl;
    continue;
  };
}

function copyAuxCont(_cellX, _prec) {
  while (true) {
    let prec = _prec;
    let cellX = _cellX;
    if (cellX === 0) {
      return prec;
    }
    let next = {
      hd: cellX.hd,
      tl: /* [] */0
    };
    prec.tl = next;
    _prec = next;
    _cellX = cellX.tl;
    continue;
  };
}

function copyAuxWitFilter(f, _cellX, _prec) {
  while (true) {
    let prec = _prec;
    let cellX = _cellX;
    if (cellX === 0) {
      return;
    }
    let t = cellX.tl;
    let h = cellX.hd;
    if (f(h)) {
      let next = {
        hd: h,
        tl: /* [] */0
      };
      prec.tl = next;
      _prec = next;
      _cellX = t;
      continue;
    }
    _cellX = t;
    continue;
  };
}

function copyAuxWithFilterIndex(f, _cellX, _prec, _i) {
  while (true) {
    let i = _i;
    let prec = _prec;
    let cellX = _cellX;
    if (cellX === 0) {
      return;
    }
    let t = cellX.tl;
    let h = cellX.hd;
    if (f(h, i)) {
      let next = {
        hd: h,
        tl: /* [] */0
      };
      prec.tl = next;
      _i = i + 1 | 0;
      _prec = next;
      _cellX = t;
      continue;
    }
    _i = i + 1 | 0;
    _cellX = t;
    continue;
  };
}

function copyAuxWitFilterMap(f, _cellX, _prec) {
  while (true) {
    let prec = _prec;
    let cellX = _cellX;
    if (cellX === 0) {
      return;
    }
    let t = cellX.tl;
    let h = f(cellX.hd);
    if (h !== undefined) {
      let next = {
        hd: Primitive_option.valFromOption(h),
        tl: /* [] */0
      };
      prec.tl = next;
      _prec = next;
      _cellX = t;
      continue;
    }
    _cellX = t;
    continue;
  };
}

function removeAssocAuxWithMap(_cellX, x, _prec, f) {
  while (true) {
    let prec = _prec;
    let cellX = _cellX;
    if (cellX === 0) {
      return false;
    }
    let t = cellX.tl;
    let h = cellX.hd;
    if (f(h[0], x)) {
      prec.tl = t;
      return true;
    }
    let next = {
      hd: h,
      tl: /* [] */0
    };
    prec.tl = next;
    _prec = next;
    _cellX = t;
    continue;
  };
}

function setAssocAuxWithMap(_cellX, x, k, _prec, eq) {
  while (true) {
    let prec = _prec;
    let cellX = _cellX;
    if (cellX === 0) {
      return false;
    }
    let t = cellX.tl;
    let h = cellX.hd;
    if (eq(h[0], x)) {
      prec.tl = {
        hd: [
          x,
          k
        ],
        tl: t
      };
      return true;
    }
    let next = {
      hd: h,
      tl: /* [] */0
    };
    prec.tl = next;
    _prec = next;
    _cellX = t;
    continue;
  };
}

function copyAuxWithMap(_cellX, _prec, f) {
  while (true) {
    let prec = _prec;
    let cellX = _cellX;
    if (cellX === 0) {
      return;
    }
    let next = {
      hd: f(cellX.hd),
      tl: /* [] */0
    };
    prec.tl = next;
    _prec = next;
    _cellX = cellX.tl;
    continue;
  };
}

function zipAux(_cellX, _cellY, _prec) {
  while (true) {
    let prec = _prec;
    let cellY = _cellY;
    let cellX = _cellX;
    if (cellX === 0) {
      return;
    }
    if (cellY === 0) {
      return;
    }
    let next = {
      hd: [
        cellX.hd,
        cellY.hd
      ],
      tl: /* [] */0
    };
    prec.tl = next;
    _prec = next;
    _cellY = cellY.tl;
    _cellX = cellX.tl;
    continue;
  };
}

function copyAuxWithMap2(f, _cellX, _cellY, _prec) {
  while (true) {
    let prec = _prec;
    let cellY = _cellY;
    let cellX = _cellX;
    if (cellX === 0) {
      return;
    }
    if (cellY === 0) {
      return;
    }
    let next = {
      hd: f(cellX.hd, cellY.hd),
      tl: /* [] */0
    };
    prec.tl = next;
    _prec = next;
    _cellY = cellY.tl;
    _cellX = cellX.tl;
    continue;
  };
}

function copyAuxWithMapI(f, _i, _cellX, _prec) {
  while (true) {
    let prec = _prec;
    let cellX = _cellX;
    let i = _i;
    if (cellX === 0) {
      return;
    }
    let next = {
      hd: f(cellX.hd, i),
      tl: /* [] */0
    };
    prec.tl = next;
    _prec = next;
    _cellX = cellX.tl;
    _i = i + 1 | 0;
    continue;
  };
}

function takeAux(_n, _cell, _prec) {
  while (true) {
    let prec = _prec;
    let cell = _cell;
    let n = _n;
    if (n === 0) {
      return true;
    }
    if (cell === 0) {
      return false;
    }
    let cell$1 = {
      hd: cell.hd,
      tl: /* [] */0
    };
    prec.tl = cell$1;
    _prec = cell$1;
    _cell = cell.tl;
    _n = n - 1 | 0;
    continue;
  };
}

function splitAtAux(_n, _cell, _prec) {
  while (true) {
    let prec = _prec;
    let cell = _cell;
    let n = _n;
    if (n === 0) {
      return cell;
    }
    if (cell === 0) {
      return;
    }
    let cell$1 = {
      hd: cell.hd,
      tl: /* [] */0
    };
    prec.tl = cell$1;
    _prec = cell$1;
    _cell = cell.tl;
    _n = n - 1 | 0;
    continue;
  };
}

function take(lst, n) {
  if (n < 0) {
    return;
  }
  if (n === 0) {
    return /* [] */0;
  }
  if (lst === 0) {
    return;
  }
  let cell = {
    hd: lst.hd,
    tl: /* [] */0
  };
  let has = takeAux(n - 1 | 0, lst.tl, cell);
  if (has) {
    return cell;
  }
  
}

function drop(lst, n) {
  if (n < 0) {
    return;
  } else {
    let _l = lst;
    let _n = n;
    while (true) {
      let n$1 = _n;
      let l = _l;
      if (n$1 === 0) {
        return l;
      }
      if (l === 0) {
        return;
      }
      _n = n$1 - 1 | 0;
      _l = l.tl;
      continue;
    };
  }
}

function splitAt(lst, n) {
  if (n < 0) {
    return;
  }
  if (n === 0) {
    return [
      /* [] */0,
      lst
    ];
  }
  if (lst === 0) {
    return;
  }
  let cell = {
    hd: lst.hd,
    tl: /* [] */0
  };
  let rest = splitAtAux(n - 1 | 0, lst.tl, cell);
  if (rest !== undefined) {
    return [
      cell,
      rest
    ];
  }
  
}

function concat(xs, ys) {
  if (xs === 0) {
    return ys;
  }
  let cell = {
    hd: xs.hd,
    tl: /* [] */0
  };
  copyAuxCont(xs.tl, cell).tl = ys;
  return cell;
}

function map(xs, f) {
  if (xs === 0) {
    return /* [] */0;
  }
  let cell = {
    hd: f(xs.hd),
    tl: /* [] */0
  };
  copyAuxWithMap(xs.tl, cell, f);
  return cell;
}

function zipBy(l1, l2, f) {
  if (l1 === 0) {
    return /* [] */0;
  }
  if (l2 === 0) {
    return /* [] */0;
  }
  let cell = {
    hd: f(l1.hd, l2.hd),
    tl: /* [] */0
  };
  copyAuxWithMap2(f, l1.tl, l2.tl, cell);
  return cell;
}

function mapWithIndex(xs, f) {
  if (xs === 0) {
    return /* [] */0;
  }
  let cell = {
    hd: f(xs.hd, 0),
    tl: /* [] */0
  };
  copyAuxWithMapI(f, 1, xs.tl, cell);
  return cell;
}

function fromInitializer(n, f) {
  if (n <= 0) {
    return /* [] */0;
  }
  let headX = {
    hd: f(0),
    tl: /* [] */0
  };
  let cur = headX;
  let i = 1;
  while (i < n) {
    let v = {
      hd: f(i),
      tl: /* [] */0
    };
    cur.tl = v;
    cur = v;
    i = i + 1 | 0;
  };
  return headX;
}

function make(n, v) {
  if (n <= 0) {
    return /* [] */0;
  }
  let headX = {
    hd: v,
    tl: /* [] */0
  };
  let cur = headX;
  let i = 1;
  while (i < n) {
    let v$1 = {
      hd: v,
      tl: /* [] */0
    };
    cur.tl = v$1;
    cur = v$1;
    i = i + 1 | 0;
  };
  return headX;
}

function length(xs) {
  let _x = xs;
  let _acc = 0;
  while (true) {
    let acc = _acc;
    let x = _x;
    if (x === 0) {
      return acc;
    }
    _acc = acc + 1 | 0;
    _x = x.tl;
    continue;
  };
}

function fillAux(arr, _i, _x) {
  while (true) {
    let x = _x;
    let i = _i;
    if (x === 0) {
      return;
    }
    arr[i] = x.hd;
    _x = x.tl;
    _i = i + 1 | 0;
    continue;
  };
}

function fromArray(a) {
  let _i = a.length - 1 | 0;
  let _res = /* [] */0;
  while (true) {
    let res = _res;
    let i = _i;
    if (i < 0) {
      return res;
    }
    _res = {
      hd: a[i],
      tl: res
    };
    _i = i - 1 | 0;
    continue;
  };
}

function toArray(x) {
  let len = length(x);
  let arr = new Array(len);
  fillAux(arr, 0, x);
  return arr;
}

function shuffle(xs) {
  let v = toArray(xs);
  Stdlib_Array.shuffle(v);
  return fromArray(v);
}

function reverseConcat(_l1, _l2) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (l1 === 0) {
      return l2;
    }
    _l2 = {
      hd: l1.hd,
      tl: l2
    };
    _l1 = l1.tl;
    continue;
  };
}

function reverse(l) {
  return reverseConcat(l, /* [] */0);
}

function flatAux(_prec, _xs) {
  while (true) {
    let xs = _xs;
    let prec = _prec;
    if (xs !== 0) {
      _xs = xs.tl;
      _prec = copyAuxCont(xs.hd, prec);
      continue;
    }
    prec.tl = /* [] */0;
    return;
  };
}

function flat(_xs) {
  while (true) {
    let xs = _xs;
    if (xs === 0) {
      return /* [] */0;
    }
    let match = xs.hd;
    if (match !== 0) {
      let cell = {
        hd: match.hd,
        tl: /* [] */0
      };
      flatAux(copyAuxCont(match.tl, cell), xs.tl);
      return cell;
    }
    _xs = xs.tl;
    continue;
  };
}

function concatMany(xs) {
  let len = xs.length;
  if (len === 1) {
    return xs[0];
  }
  if (len === 0) {
    return /* [] */0;
  }
  let len$1 = xs.length;
  let v = xs[len$1 - 1 | 0];
  for (let i = len$1 - 2 | 0; i >= 0; --i) {
    v = concat(xs[i], v);
  }
  return v;
}

function mapReverse(l, f) {
  let _accu = /* [] */0;
  let _xs = l;
  while (true) {
    let xs = _xs;
    let accu = _accu;
    if (xs === 0) {
      return accu;
    }
    _xs = xs.tl;
    _accu = {
      hd: f(xs.hd),
      tl: accu
    };
    continue;
  };
}

function forEach(_xs, f) {
  while (true) {
    let xs = _xs;
    if (xs === 0) {
      return;
    }
    f(xs.hd);
    _xs = xs.tl;
    continue;
  };
}

function forEachWithIndex(l, f) {
  let _xs = l;
  let _i = 0;
  while (true) {
    let i = _i;
    let xs = _xs;
    if (xs === 0) {
      return;
    }
    f(xs.hd, i);
    _i = i + 1 | 0;
    _xs = xs.tl;
    continue;
  };
}

function reduce(_l, _accu, f) {
  while (true) {
    let accu = _accu;
    let l = _l;
    if (l === 0) {
      return accu;
    }
    _accu = f(accu, l.hd);
    _l = l.tl;
    continue;
  };
}

function reduceReverseUnsafe(l, accu, f) {
  if (l !== 0) {
    return f(reduceReverseUnsafe(l.tl, accu, f), l.hd);
  } else {
    return accu;
  }
}

function reduceReverse(l, acc, f) {
  let len = length(l);
  if (len < 1000) {
    return reduceReverseUnsafe(l, acc, f);
  } else {
    let a = toArray(l);
    let r = acc;
    for (let i = a.length - 1 | 0; i >= 0; --i) {
      r = f(r, a[i]);
    }
    return r;
  }
}

function reduceWithIndex(l, acc, f) {
  let _l = l;
  let _acc = acc;
  let _i = 0;
  while (true) {
    let i = _i;
    let acc$1 = _acc;
    let l$1 = _l;
    if (l$1 === 0) {
      return acc$1;
    }
    _i = i + 1 | 0;
    _acc = f(acc$1, l$1.hd, i);
    _l = l$1.tl;
    continue;
  };
}

function mapReverse2(l1, l2, f) {
  let _l1 = l1;
  let _l2 = l2;
  let _accu = /* [] */0;
  while (true) {
    let accu = _accu;
    let l2$1 = _l2;
    let l1$1 = _l1;
    if (l1$1 === 0) {
      return accu;
    }
    if (l2$1 === 0) {
      return accu;
    }
    _accu = {
      hd: f(l1$1.hd, l2$1.hd),
      tl: accu
    };
    _l2 = l2$1.tl;
    _l1 = l1$1.tl;
    continue;
  };
}

function forEach2(_l1, _l2, f) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (l1 === 0) {
      return;
    }
    if (l2 === 0) {
      return;
    }
    f(l1.hd, l2.hd);
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue;
  };
}

function reduce2(_l1, _l2, _accu, f) {
  while (true) {
    let accu = _accu;
    let l2 = _l2;
    let l1 = _l1;
    if (l1 === 0) {
      return accu;
    }
    if (l2 === 0) {
      return accu;
    }
    _accu = f(accu, l1.hd, l2.hd);
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue;
  };
}

function reduceReverse2Unsafe(l1, l2, accu, f) {
  if (l1 !== 0 && l2 !== 0) {
    return f(reduceReverse2Unsafe(l1.tl, l2.tl, accu, f), l1.hd, l2.hd);
  } else {
    return accu;
  }
}

function reduceReverse2(l1, l2, acc, f) {
  let len = length(l1);
  if (len < 1000) {
    return reduceReverse2Unsafe(l1, l2, acc, f);
  } else {
    let a = toArray(l1);
    let b = toArray(l2);
    let r = acc;
    let len$1 = Primitive_int.min(a.length, b.length);
    for (let i = len$1 - 1 | 0; i >= 0; --i) {
      r = f(r, a[i], b[i]);
    }
    return r;
  }
}

function every(_xs, p) {
  while (true) {
    let xs = _xs;
    if (xs === 0) {
      return true;
    }
    if (!p(xs.hd)) {
      return false;
    }
    _xs = xs.tl;
    continue;
  };
}

function some(_xs, p) {
  while (true) {
    let xs = _xs;
    if (xs === 0) {
      return false;
    }
    if (p(xs.hd)) {
      return true;
    }
    _xs = xs.tl;
    continue;
  };
}

function every2(_l1, _l2, p) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (l1 === 0) {
      return true;
    }
    if (l2 === 0) {
      return true;
    }
    if (!p(l1.hd, l2.hd)) {
      return false;
    }
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue;
  };
}

function compareLength(_l1, _l2) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (l1 === 0) {
      if (l2 !== 0) {
        return -1;
      } else {
        return 0;
      }
    }
    if (l2 === 0) {
      return 1;
    }
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue;
  };
}

function compare(_l1, _l2, p) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (l1 === 0) {
      if (l2 !== 0) {
        return -1;
      } else {
        return 0;
      }
    }
    if (l2 === 0) {
      return 1;
    }
    let c = p(l1.hd, l2.hd);
    if (c !== 0) {
      return c;
    }
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue;
  };
}

function equal(_l1, _l2, p) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (l1 === 0) {
      return l2 === 0;
    }
    if (l2 === 0) {
      return false;
    }
    if (!p(l1.hd, l2.hd)) {
      return false;
    }
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue;
  };
}

function some2(_l1, _l2, p) {
  while (true) {
    let l2 = _l2;
    let l1 = _l1;
    if (l1 === 0) {
      return false;
    }
    if (l2 === 0) {
      return false;
    }
    if (p(l1.hd, l2.hd)) {
      return true;
    }
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue;
  };
}

function has(_xs, x, eq) {
  while (true) {
    let xs = _xs;
    if (xs === 0) {
      return false;
    }
    if (eq(xs.hd, x)) {
      return true;
    }
    _xs = xs.tl;
    continue;
  };
}

function getAssoc(_xs, x, eq) {
  while (true) {
    let xs = _xs;
    if (xs === 0) {
      return;
    }
    let match = xs.hd;
    if (eq(match[0], x)) {
      return Primitive_option.some(match[1]);
    }
    _xs = xs.tl;
    continue;
  };
}

function hasAssoc(_xs, x, eq) {
  while (true) {
    let xs = _xs;
    if (xs === 0) {
      return false;
    }
    if (eq(xs.hd[0], x)) {
      return true;
    }
    _xs = xs.tl;
    continue;
  };
}

function removeAssoc(xs, x, eq) {
  if (xs === 0) {
    return /* [] */0;
  }
  let l = xs.tl;
  let pair = xs.hd;
  if (eq(pair[0], x)) {
    return l;
  }
  let cell = {
    hd: pair,
    tl: /* [] */0
  };
  let removed = removeAssocAuxWithMap(l, x, cell, eq);
  if (removed) {
    return cell;
  } else {
    return xs;
  }
}

function setAssoc(xs, x, k, eq) {
  if (xs === 0) {
    return {
      hd: [
        x,
        k
      ],
      tl: /* [] */0
    };
  }
  let l = xs.tl;
  let pair = xs.hd;
  if (eq(pair[0], x)) {
    return {
      hd: [
        x,
        k
      ],
      tl: l
    };
  }
  let cell = {
    hd: pair,
    tl: /* [] */0
  };
  let replaced = setAssocAuxWithMap(l, x, k, cell, eq);
  if (replaced) {
    return cell;
  } else {
    return {
      hd: [
        x,
        k
      ],
      tl: xs
    };
  }
}

function sort(xs, cmp) {
  let arr = toArray(xs);
  arr.sort(cmp);
  return fromArray(arr);
}

function find(_xs, p) {
  while (true) {
    let xs = _xs;
    if (xs === 0) {
      return;
    }
    let x = xs.hd;
    if (p(x)) {
      return Primitive_option.some(x);
    }
    _xs = xs.tl;
    continue;
  };
}

function filter(_xs, p) {
  while (true) {
    let xs = _xs;
    if (xs === 0) {
      return /* [] */0;
    }
    let t = xs.tl;
    let h = xs.hd;
    if (p(h)) {
      let cell = {
        hd: h,
        tl: /* [] */0
      };
      copyAuxWitFilter(p, t, cell);
      return cell;
    }
    _xs = t;
    continue;
  };
}

function filterWithIndex(xs, p) {
  let _xs = xs;
  let _i = 0;
  while (true) {
    let i = _i;
    let xs$1 = _xs;
    if (xs$1 === 0) {
      return /* [] */0;
    }
    let t = xs$1.tl;
    let h = xs$1.hd;
    if (p(h, i)) {
      let cell = {
        hd: h,
        tl: /* [] */0
      };
      copyAuxWithFilterIndex(p, t, cell, i + 1 | 0);
      return cell;
    }
    _i = i + 1 | 0;
    _xs = t;
    continue;
  };
}

function filterMap(_xs, p) {
  while (true) {
    let xs = _xs;
    if (xs === 0) {
      return /* [] */0;
    }
    let t = xs.tl;
    let h = p(xs.hd);
    if (h !== undefined) {
      let cell = {
        hd: Primitive_option.valFromOption(h),
        tl: /* [] */0
      };
      copyAuxWitFilterMap(p, t, cell);
      return cell;
    }
    _xs = t;
    continue;
  };
}

function partition(l, p) {
  if (l === 0) {
    return [
      /* [] */0,
      /* [] */0
    ];
  }
  let h = l.hd;
  let nextX = {
    hd: h,
    tl: /* [] */0
  };
  let nextY = {
    hd: h,
    tl: /* [] */0
  };
  let b = p(h);
  partitionAux(p, l.tl, nextX, nextY);
  if (b) {
    return [
      nextX,
      nextY.tl
    ];
  } else {
    return [
      nextX.tl,
      nextY
    ];
  }
}

function unzip(xs) {
  if (xs === 0) {
    return [
      /* [] */0,
      /* [] */0
    ];
  }
  let match = xs.hd;
  let cellX = {
    hd: match[0],
    tl: /* [] */0
  };
  let cellY = {
    hd: match[1],
    tl: /* [] */0
  };
  splitAux(xs.tl, cellX, cellY);
  return [
    cellX,
    cellY
  ];
}

function zip(l1, l2) {
  if (l1 === 0) {
    return /* [] */0;
  }
  if (l2 === 0) {
    return /* [] */0;
  }
  let cell = {
    hd: [
      l1.hd,
      l2.hd
    ],
    tl: /* [] */0
  };
  zipAux(l1.tl, l2.tl, cell);
  return cell;
}

let size = length;

let toShuffled = shuffle;

export {
  length,
  size,
  head,
  headExn,
  tail,
  tailExn,
  add,
  get,
  getExn,
  make,
  fromInitializer,
  shuffle,
  toShuffled,
  drop,
  take,
  splitAt,
  concat,
  concatMany,
  reverseConcat,
  flat,
  map,
  zip,
  zipBy,
  mapWithIndex,
  fromArray,
  toArray,
  reverse,
  mapReverse,
  forEach,
  forEachWithIndex,
  reduce,
  reduceWithIndex,
  reduceReverse,
  mapReverse2,
  forEach2,
  reduce2,
  reduceReverse2,
  every,
  some,
  every2,
  some2,
  compareLength,
  compare,
  equal,
  has,
  find,
  filter,
  filterWithIndex,
  filterMap,
  partition,
  unzip,
  getAssoc,
  hasAssoc,
  removeAssoc,
  setAssoc,
  sort,
}
/* No side effect */

'use strict';

let Primitive_option = require("./Primitive_option.js");

function make() {
  return {
    length: 0,
    first: undefined,
    last: undefined
  };
}

function clear(q) {
  q.length = 0;
  q.first = undefined;
  q.last = undefined;
}

function add(q, x) {
  let cell = {
    content: x,
    next: undefined
  };
  let last = q.last;
  if (last !== undefined) {
    q.length = q.length + 1 | 0;
    last.next = cell;
    q.last = cell;
  } else {
    q.length = 1;
    q.first = cell;
    q.last = cell;
  }
}

function peek(q) {
  let v = q.first;
  if (v !== undefined) {
    return Primitive_option.some(v.content);
  }
}

function peekUndefined(q) {
  let v = q.first;
  if (v !== undefined) {
    return v.content;
  }
}

function peekOrThrow(q) {
  let v = q.first;
  if (v !== undefined) {
    return v.content;
  }
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}

function pop(q) {
  let x = q.first;
  if (x === undefined) {
    return;
  }
  let next = x.next;
  if (next === undefined) {
    clear(q);
    return Primitive_option.some(x.content);
  } else {
    q.length = q.length - 1 | 0;
    q.first = next;
    return Primitive_option.some(x.content);
  }
}

function popOrThrow(q) {
  let x = q.first;
  if (x !== undefined) {
    let next = x.next;
    if (next === undefined) {
      clear(q);
      return x.content;
    } else {
      q.length = q.length - 1 | 0;
      q.first = next;
      return x.content;
    }
  }
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}

function popUndefined(q) {
  let x = q.first;
  if (x === undefined) {
    return;
  }
  let next = x.next;
  if (next === undefined) {
    clear(q);
    return x.content;
  } else {
    q.length = q.length - 1 | 0;
    q.first = next;
    return x.content;
  }
}

function copy(q) {
  let qRes = {
    length: q.length,
    first: undefined,
    last: undefined
  };
  let _prev;
  let _cell = q.first;
  while (true) {
    let cell = _cell;
    let prev = _prev;
    if (cell !== undefined) {
      let content = cell.content;
      let res = {
        content: content,
        next: undefined
      };
      if (prev !== undefined) {
        prev.next = res;
      } else {
        qRes.first = res;
      }
      _cell = cell.next;
      _prev = res;
      continue;
    }
    qRes.last = prev;
    return qRes;
  };
}

function map(q, f) {
  let qRes = {
    length: q.length,
    first: undefined,
    last: undefined
  };
  let _prev;
  let _cell = q.first;
  while (true) {
    let cell = _cell;
    let prev = _prev;
    if (cell !== undefined) {
      let content = f(cell.content);
      let res = {
        content: content,
        next: undefined
      };
      if (prev !== undefined) {
        prev.next = res;
      } else {
        qRes.first = res;
      }
      _cell = cell.next;
      _prev = res;
      continue;
    }
    qRes.last = prev;
    return qRes;
  };
}

function isEmpty(q) {
  return q.length === 0;
}

function size(q) {
  return q.length;
}

function forEach(q, f) {
  let _cell = q.first;
  while (true) {
    let cell = _cell;
    if (cell === undefined) {
      return;
    }
    f(cell.content);
    _cell = cell.next;
    continue;
  };
}

function reduce(q, accu, f) {
  let _accu = accu;
  let _cell = q.first;
  while (true) {
    let cell = _cell;
    let accu$1 = _accu;
    if (cell === undefined) {
      return accu$1;
    }
    let accu$2 = f(accu$1, cell.content);
    _cell = cell.next;
    _accu = accu$2;
    continue;
  };
}

function transfer(q1, q2) {
  if (q1.length <= 0) {
    return;
  }
  let l = q2.last;
  if (l !== undefined) {
    q2.length = q2.length + q1.length | 0;
    l.next = q1.first;
    q2.last = q1.last;
    return clear(q1);
  } else {
    q2.length = q1.length;
    q2.first = q1.first;
    q2.last = q1.last;
    return clear(q1);
  }
}

function fillAux(_i, arr, _cell) {
  while (true) {
    let cell = _cell;
    let i = _i;
    if (cell === undefined) {
      return;
    }
    arr[i] = cell.content;
    _cell = cell.next;
    _i = i + 1 | 0;
    continue;
  };
}

function toArray(x) {
  let v = new Array(x.length);
  fillAux(0, v, x.first);
  return v;
}

function fromArray(arr) {
  let q = {
    length: 0,
    first: undefined,
    last: undefined
  };
  for (let i = 0, i_finish = arr.length; i < i_finish; ++i) {
    add(q, arr[i]);
  }
  return q;
}

let peekExn = peekOrThrow;

let popExn = popOrThrow;

let mapU = map;

let forEachU = forEach;

let reduceU = reduce;

exports.make = make;
exports.clear = clear;
exports.isEmpty = isEmpty;
exports.fromArray = fromArray;
exports.add = add;
exports.peek = peek;
exports.peekUndefined = peekUndefined;
exports.peekExn = peekExn;
exports.peekOrThrow = peekOrThrow;
exports.pop = pop;
exports.popUndefined = popUndefined;
exports.popExn = popExn;
exports.popOrThrow = popOrThrow;
exports.copy = copy;
exports.size = size;
exports.mapU = mapU;
exports.map = map;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.transfer = transfer;
exports.toArray = toArray;
/* No side effect */

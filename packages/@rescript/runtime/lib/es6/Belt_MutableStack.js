

import * as Primitive_option from "./Primitive_option.js";

function make() {
  return {
    root: undefined
  };
}

function clear(s) {
  s.root = undefined;
}

function copy(s) {
  return {
    root: s.root
  };
}

function push(s, x) {
  s.root = {
    head: x,
    tail: s.root
  };
}

function topUndefined(s) {
  let x = s.root;
  if (x !== undefined) {
    return x.head;
  }
}

function top(s) {
  let x = s.root;
  if (x !== undefined) {
    return Primitive_option.some(x.head);
  }
}

function isEmpty(s) {
  return s.root === undefined;
}

function popUndefined(s) {
  let x = s.root;
  if (x !== undefined) {
    s.root = x.tail;
    return x.head;
  }
}

function pop(s) {
  let x = s.root;
  if (x !== undefined) {
    s.root = x.tail;
    return Primitive_option.some(x.head);
  }
}

function size(s) {
  let x = s.root;
  if (x !== undefined) {
    let _x = x;
    let _acc = 0;
    while (true) {
      let acc = _acc;
      let x$1 = _x;
      let x$2 = x$1.tail;
      if (x$2 === undefined) {
        return acc + 1 | 0;
      }
      _acc = acc + 1 | 0;
      _x = x$2;
      continue;
    };
  } else {
    return 0;
  }
}

function forEach(s, f) {
  let _s = s.root;
  while (true) {
    let s$1 = _s;
    if (s$1 === undefined) {
      return;
    }
    f(s$1.head);
    _s = s$1.tail;
    continue;
  };
}

function dynamicPopIter(s, f) {
  while (true) {
    let match = s.root;
    if (match === undefined) {
      return;
    }
    s.root = match.tail;
    f(match.head);
    continue;
  };
}

let forEachU = forEach;

let dynamicPopIterU = dynamicPopIter;

export {
  make,
  clear,
  copy,
  push,
  popUndefined,
  pop,
  topUndefined,
  top,
  isEmpty,
  size,
  forEachU,
  forEach,
  dynamicPopIterU,
  dynamicPopIter,
}
/* No side effect */

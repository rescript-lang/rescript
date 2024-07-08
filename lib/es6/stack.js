

import * as List from "./list.js";
import * as Caml_exceptions from "./caml_exceptions.js";

let Empty = /* @__PURE__ */Caml_exceptions.create("Stack.Empty");

function create() {
  return {
    c: /* [] */0,
    len: 0
  };
}

function clear(s) {
  s.c = /* [] */0;
  s.len = 0;
}

function copy(s) {
  return {
    c: s.c,
    len: s.len
  };
}

function push(x, s) {
  s.c = {
    hd: x,
    tl: s.c
  };
  s.len = s.len + 1 | 0;
}

function pop(s) {
  let match = s.c;
  if (match) {
    s.c = match.tl;
    s.len = s.len - 1 | 0;
    return match.hd;
  }
  throw new Error(Empty, {
        cause: {
          RE_EXN_ID: Empty
        }
      });
}

function top(s) {
  let match = s.c;
  if (match) {
    return match.hd;
  }
  throw new Error(Empty, {
        cause: {
          RE_EXN_ID: Empty
        }
      });
}

function is_empty(s) {
  return s.c === /* [] */0;
}

function length(s) {
  return s.len;
}

function iter(f, s) {
  List.iter(f, s.c);
}

function fold(f, acc, s) {
  return List.fold_left(f, acc, s.c);
}

export {
  Empty,
  create,
  push,
  pop,
  top,
  clear,
  copy,
  is_empty,
  length,
  iter,
  fold,
}
/* No side effect */

'use strict';

let Caml_exceptions = require("./caml_exceptions.js");
let Caml_js_exceptions = require("./caml_js_exceptions.js");

let Empty = /* @__PURE__ */Caml_exceptions.create("Queue.Empty");

function create() {
  return {
    length: 0,
    first: "Nil",
    last: "Nil"
  };
}

function clear(q) {
  q.length = 0;
  q.first = "Nil";
  q.last = "Nil";
}

function add(x, q) {
  let cell = {
    TAG: "Cons",
    content: x,
    next: "Nil"
  };
  let last = q.last;
  if (typeof last !== "object") {
    q.length = 1;
    q.first = cell;
    q.last = cell;
    return;
  }
  q.length = q.length + 1 | 0;
  last.next = cell;
  q.last = cell;
}

function peek(q) {
  let match = q.first;
  if (typeof match === "object") {
    return match.content;
  }
  throw Caml_js_exceptions.internalMakeExn(Empty);
}

function take(q) {
  let match = q.first;
  if (typeof match !== "object") {
    throw Caml_js_exceptions.internalMakeExn(Empty);
  }
  let content = match.content;
  let next = match.next;
  if (typeof next !== "object") {
    clear(q);
    return content;
  }
  q.length = q.length - 1 | 0;
  q.first = next;
  return content;
}

function copy(q) {
  let q_res = {
    length: q.length,
    first: "Nil",
    last: "Nil"
  };
  let _prev = "Nil";
  let _cell = q.first;
  while (true) {
    let cell = _cell;
    let prev = _prev;
    if (typeof cell !== "object") {
      q_res.last = prev;
      return q_res;
    }
    let next = cell.next;
    let res = {
      TAG: "Cons",
      content: cell.content,
      next: "Nil"
    };
    if (typeof prev !== "object") {
      q_res.first = res;
    } else {
      prev.next = res;
    }
    _cell = next;
    _prev = res;
    continue;
  };
}

function is_empty(q) {
  return q.length === 0;
}

function length(q) {
  return q.length;
}

function iter(f, q) {
  let _cell = q.first;
  while (true) {
    let cell = _cell;
    if (typeof cell !== "object") {
      return;
    }
    let next = cell.next;
    f(cell.content);
    _cell = next;
    continue;
  };
}

function fold(f, accu, q) {
  let _accu = accu;
  let _cell = q.first;
  while (true) {
    let cell = _cell;
    let accu$1 = _accu;
    if (typeof cell !== "object") {
      return accu$1;
    }
    let next = cell.next;
    let accu$2 = f(accu$1, cell.content);
    _cell = next;
    _accu = accu$2;
    continue;
  };
}

function transfer(q1, q2) {
  if (q1.length <= 0) {
    return;
  }
  let last = q2.last;
  if (typeof last !== "object") {
    q2.length = q1.length;
    q2.first = q1.first;
    q2.last = q1.last;
    return clear(q1);
  }
  q2.length = q2.length + q1.length | 0;
  last.next = q1.first;
  q2.last = q1.last;
  clear(q1);
}

let push = add;

let pop = take;

let top = peek;

exports.Empty = Empty;
exports.create = create;
exports.add = add;
exports.push = push;
exports.take = take;
exports.pop = pop;
exports.peek = peek;
exports.top = top;
exports.clear = clear;
exports.copy = copy;
exports.is_empty = is_empty;
exports.length = length;
exports.iter = iter;
exports.fold = fold;
exports.transfer = transfer;
/* No side effect */

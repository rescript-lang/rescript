// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Belt_List = require("../../lib/js/belt_List.js");
let Primitive_object = require("../../lib/js/primitive_object.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

let rec_cell = {};

rec_cell.content = 3;

rec_cell.next = rec_cell;

function f0(x) {
  let rec_cell = {};
  Primitive_object.update_dummy(rec_cell, {
    content: Math.imul(x, x) - 6 | 0,
    next: rec_cell
  });
  return rec_cell;
}

function a0(x) {
  return (x.content + x.next.content | 0) + x.next.next.content | 0;
}

eq("File \"recursive_records_test.res\", line 26, characters 5-12", a0(rec_cell), 9);

eq("File \"recursive_records_test.res\", line 27, characters 5-12", a0(f0(3)), 9);

let rec_cell2 = {};

rec_cell2.content = 3;

rec_cell2.next = rec_cell2;

function f2(x) {
  let rec_cell2 = {};
  Primitive_object.update_dummy(rec_cell2, {
    TAG: "Cons",
    content: Math.imul(x, x) - 6 | 0,
    next: rec_cell2
  });
  return rec_cell2;
}

function hd(x) {
  if (typeof x !== "object") {
    return 0;
  } else {
    return x.content;
  }
}

function tl_exn(x) {
  if (typeof x === "object") {
    return x.next;
  }
  throw {
    RE_EXN_ID: "Assert_failure",
    _1: [
      "recursive_records_test.res",
      49,
      11
    ],
    Error: new Error()
  };
}

eq("File \"recursive_records_test.res\", line 54, characters 5-12", (hd(rec_cell2) + hd(tl_exn(rec_cell2)) | 0) + hd(tl_exn(tl_exn(rec_cell2))) | 0, 9);

let rec_cell2$1 = f2(3);

eq("File \"recursive_records_test.res\", line 56, characters 5-12", (hd(rec_cell2$1) + hd(tl_exn(rec_cell2$1)) | 0) + hd(tl_exn(tl_exn(rec_cell2$1))) | 0, 9);

let rec_cell3 = {};

rec_cell3.hd = 3;

rec_cell3.tl = rec_cell3;

function f3(x) {
  let rec_cell3 = {};
  Primitive_object.update_dummy(rec_cell3, {
    hd: Math.imul(x, x) - 6 | 0,
    tl: rec_cell3
  });
  return rec_cell3;
}

eq("File \"recursive_records_test.res\", line 68, characters 4-11", (Belt_List.headExn(rec_cell3) + Belt_List.headExn(Belt_List.tailExn(rec_cell3)) | 0) + Belt_List.headExn(Belt_List.tailExn(Belt_List.tailExn(rec_cell3))) | 0, 9);

let rec_cell3$1 = f3(3);

eq("File \"recursive_records_test.res\", line 78, characters 4-11", (Belt_List.headExn(rec_cell3$1) + Belt_List.headExn(Belt_List.tailExn(rec_cell3$1)) | 0) + Belt_List.headExn(Belt_List.tailExn(Belt_List.tailExn(rec_cell3$1))) | 0, 9);

Mt.from_pair_suites("recursive_records_test.res", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.rec_cell = rec_cell;
exports.f0 = f0;
exports.a0 = a0;
exports.rec_cell2 = rec_cell2;
exports.f2 = f2;
exports.hd = hd;
exports.tl_exn = tl_exn;
exports.rec_cell3 = rec_cell3;
exports.f3 = f3;
/*  Not a pure module */

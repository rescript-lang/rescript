'use strict';

var Mt = require("./mt.js");
var Caml = require("../../lib/js/caml.js");
var Belt_Id = require("../../lib/js/belt_Id.js");
var Belt_List = require("../../lib/js/belt_List.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Array_data_util = require("./array_data_util.js");
var Belt_MutableSet = require("../../lib/js/belt_MutableSet.js");
var Belt_internalAVLset = require("../../lib/js/belt_internalAVLset.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  Mt.bool_suites(test_id, suites, loc, x);
}

var IntCmp = Belt_Id.comparable(Caml.int_compare);

function fromArray(param) {
  return Belt_MutableSet.fromArray(param, IntCmp);
}

function empty(param) {
  return {
          cmp: IntCmp.cmp,
          data: undefined
        };
}

var u = fromArray(Array_data_util.range(0, 30));

b("File \"bs_poly_mutable_set_test.res\", line 16, characters 4-11", Belt_MutableSet.removeCheck(u, 0));

b("File \"bs_poly_mutable_set_test.res\", line 17, characters 4-11", !Belt_MutableSet.removeCheck(u, 0));

b("File \"bs_poly_mutable_set_test.res\", line 18, characters 4-11", Belt_MutableSet.removeCheck(u, 30));

b("File \"bs_poly_mutable_set_test.res\", line 19, characters 4-11", Belt_MutableSet.removeCheck(u, 20));

eq("File \"bs_poly_mutable_set_test.res\", line 20, characters 5-12", Belt_internalAVLset.size(u.data), 28);

var r = Array_data_util.randomRange(0, 30);

b("File \"bs_poly_mutable_set_test.res\", line 22, characters 4-11", 29 === Belt_internalAVLset.maxUndefined(u.data));

b("File \"bs_poly_mutable_set_test.res\", line 23, characters 4-11", 1 === Belt_internalAVLset.minUndefined(u.data));

Belt_MutableSet.add(u, 3);

for(var i = 0 ,i_finish = r.length; i < i_finish; ++i){
  Belt_MutableSet.remove(u, r[i]);
}

b("File \"bs_poly_mutable_set_test.res\", line 28, characters 4-11", Belt_MutableSet.isEmpty(u));

Belt_MutableSet.add(u, 0);

Belt_MutableSet.add(u, 1);

Belt_MutableSet.add(u, 2);

Belt_MutableSet.add(u, 0);

eq("File \"bs_poly_mutable_set_test.res\", line 33, characters 5-12", Belt_internalAVLset.size(u.data), 3);

b("File \"bs_poly_mutable_set_test.res\", line 34, characters 4-11", !Belt_MutableSet.isEmpty(u));

for(var i$1 = 0; i$1 <= 3; ++i$1){
  Belt_MutableSet.remove(u, i$1);
}

b("File \"bs_poly_mutable_set_test.res\", line 38, characters 4-11", Belt_MutableSet.isEmpty(u));

Belt_MutableSet.mergeMany(u, Array_data_util.randomRange(0, 20000));

Belt_MutableSet.mergeMany(u, Array_data_util.randomRange(0, 200));

eq("File \"bs_poly_mutable_set_test.res\", line 41, characters 5-12", Belt_internalAVLset.size(u.data), 20001);

Belt_MutableSet.removeMany(u, Array_data_util.randomRange(0, 200));

eq("File \"bs_poly_mutable_set_test.res\", line 43, characters 5-12", Belt_internalAVLset.size(u.data), 19800);

Belt_MutableSet.removeMany(u, Array_data_util.randomRange(0, 1000));

eq("File \"bs_poly_mutable_set_test.res\", line 45, characters 5-12", Belt_internalAVLset.size(u.data), 19000);

Belt_MutableSet.removeMany(u, Array_data_util.randomRange(0, 1000));

eq("File \"bs_poly_mutable_set_test.res\", line 47, characters 5-12", Belt_internalAVLset.size(u.data), 19000);

Belt_MutableSet.removeMany(u, Array_data_util.randomRange(1000, 10000));

eq("File \"bs_poly_mutable_set_test.res\", line 49, characters 5-12", Belt_internalAVLset.size(u.data), 10000);

Belt_MutableSet.removeMany(u, Array_data_util.randomRange(10000, 19999));

eq("File \"bs_poly_mutable_set_test.res\", line 51, characters 5-12", Belt_internalAVLset.size(u.data), 1);

b("File \"bs_poly_mutable_set_test.res\", line 52, characters 4-11", Belt_MutableSet.has(u, 20000));

Belt_MutableSet.removeMany(u, Array_data_util.randomRange(10000, 30000));

b("File \"bs_poly_mutable_set_test.res\", line 54, characters 4-11", Belt_MutableSet.isEmpty(u));

var v = fromArray(Array_data_util.randomRange(1000, 2000));

var bs = Belt_Array.map(Array_data_util.randomRange(500, 1499), (function (x) {
        return Belt_MutableSet.removeCheck(v, x);
      }));

var indeedRemoved = Belt_Array.reduce(bs, 0, (function (acc, x) {
        if (x) {
          return acc + 1 | 0;
        } else {
          return acc;
        }
      }));

eq("File \"bs_poly_mutable_set_test.res\", line 67, characters 5-12", indeedRemoved, 500);

eq("File \"bs_poly_mutable_set_test.res\", line 68, characters 5-12", Belt_internalAVLset.size(v.data), 501);

var cs = Belt_Array.map(Array_data_util.randomRange(500, 2000), (function (x) {
        return Belt_MutableSet.addCheck(v, x);
      }));

var indeedAded = Belt_Array.reduce(cs, 0, (function (acc, x) {
        if (x) {
          return acc + 1 | 0;
        } else {
          return acc;
        }
      }));

eq("File \"bs_poly_mutable_set_test.res\", line 77, characters 5-12", indeedAded, 1000);

eq("File \"bs_poly_mutable_set_test.res\", line 78, characters 5-12", Belt_internalAVLset.size(v.data), 1501);

b("File \"bs_poly_mutable_set_test.res\", line 79, characters 4-11", Belt_MutableSet.isEmpty({
          cmp: IntCmp.cmp,
          data: undefined
        }));

eq("File \"bs_poly_mutable_set_test.res\", line 80, characters 5-12", Belt_internalAVLset.minimum(v.data), 500);

eq("File \"bs_poly_mutable_set_test.res\", line 81, characters 5-12", Belt_internalAVLset.maximum(v.data), 2000);

eq("File \"bs_poly_mutable_set_test.res\", line 82, characters 5-12", Belt_internalAVLset.minUndefined(v.data), 500);

eq("File \"bs_poly_mutable_set_test.res\", line 83, characters 5-12", Belt_internalAVLset.maxUndefined(v.data), 2000);

eq("File \"bs_poly_mutable_set_test.res\", line 84, characters 5-12", Belt_MutableSet.reduce(v, 0, (function (x, y) {
            return x + y | 0;
          })), 1876250);

b("File \"bs_poly_mutable_set_test.res\", line 85, characters 4-11", Belt_List.eq(Belt_internalAVLset.toList(v.data), Belt_List.makeBy(1501, (function (i) {
                return i + 500 | 0;
              })), (function (x, y) {
            return x === y;
          })));

eq("File \"bs_poly_mutable_set_test.res\", line 86, characters 5-12", Belt_internalAVLset.toArray(v.data), Array_data_util.range(500, 2000));

Belt_internalAVLset.checkInvariantInternal(v.data);

eq("File \"bs_poly_mutable_set_test.res\", line 88, characters 5-12", Belt_MutableSet.get(v, 3), undefined);

eq("File \"bs_poly_mutable_set_test.res\", line 89, characters 5-12", Belt_MutableSet.get(v, 1200), 1200);

var match = Belt_MutableSet.split(v, 1000);

var match$1 = match[0];

var bb = match$1[1];

var aa = match$1[0];

b("File \"bs_poly_mutable_set_test.res\", line 91, characters 4-11", match[1]);

b("File \"bs_poly_mutable_set_test.res\", line 92, characters 4-11", Belt_Array.eq(Belt_internalAVLset.toArray(aa.data), Array_data_util.range(500, 999), (function (prim0, prim1) {
            return prim0 === prim1;
          })));

b("File \"bs_poly_mutable_set_test.res\", line 93, characters 4-11", Belt_Array.eq(Belt_internalAVLset.toArray(bb.data), Array_data_util.range(1001, 2000), (function (prim0, prim1) {
            return prim0 === prim1;
          })));

b("File \"bs_poly_mutable_set_test.res\", line 94, characters 4-11", Belt_MutableSet.subset(aa, v));

b("File \"bs_poly_mutable_set_test.res\", line 95, characters 4-11", Belt_MutableSet.subset(bb, v));

b("File \"bs_poly_mutable_set_test.res\", line 96, characters 4-11", Belt_MutableSet.isEmpty(Belt_MutableSet.intersect(aa, bb)));

var c = Belt_MutableSet.removeCheck(v, 1000);

b("File \"bs_poly_mutable_set_test.res\", line 98, characters 4-11", c);

var match$2 = Belt_MutableSet.split(v, 1000);

var match$3 = match$2[0];

var bb$1 = match$3[1];

var aa$1 = match$3[0];

b("File \"bs_poly_mutable_set_test.res\", line 100, characters 4-11", !match$2[1]);

b("File \"bs_poly_mutable_set_test.res\", line 101, characters 4-11", Belt_Array.eq(Belt_internalAVLset.toArray(aa$1.data), Array_data_util.range(500, 999), (function (prim0, prim1) {
            return prim0 === prim1;
          })));

b("File \"bs_poly_mutable_set_test.res\", line 102, characters 4-11", Belt_Array.eq(Belt_internalAVLset.toArray(bb$1.data), Array_data_util.range(1001, 2000), (function (prim0, prim1) {
            return prim0 === prim1;
          })));

b("File \"bs_poly_mutable_set_test.res\", line 103, characters 4-11", Belt_MutableSet.subset(aa$1, v));

b("File \"bs_poly_mutable_set_test.res\", line 104, characters 4-11", Belt_MutableSet.subset(bb$1, v));

b("File \"bs_poly_mutable_set_test.res\", line 105, characters 4-11", Belt_MutableSet.isEmpty(Belt_MutableSet.intersect(aa$1, bb$1)));

var aa$2 = fromArray(Array_data_util.randomRange(0, 100));

var bb$2 = fromArray(Array_data_util.randomRange(40, 120));

var cc = Belt_MutableSet.union(aa$2, bb$2);

b("File \"bs_poly_mutable_set_test.res\", line 115, characters 4-11", Belt_MutableSet.eq(cc, fromArray(Array_data_util.randomRange(0, 120))));

b("File \"bs_poly_mutable_set_test.res\", line 118, characters 4-11", Belt_MutableSet.eq(Belt_MutableSet.union(fromArray(Array_data_util.randomRange(0, 20)), fromArray(Array_data_util.randomRange(21, 40))), fromArray(Array_data_util.randomRange(0, 40))));

var dd = Belt_MutableSet.intersect(aa$2, bb$2);

b("File \"bs_poly_mutable_set_test.res\", line 122, characters 4-11", Belt_MutableSet.eq(dd, fromArray(Array_data_util.randomRange(40, 100))));

b("File \"bs_poly_mutable_set_test.res\", line 124, characters 4-11", Belt_MutableSet.eq(Belt_MutableSet.intersect(fromArray(Array_data_util.randomRange(0, 20)), fromArray(Array_data_util.randomRange(21, 40))), {
          cmp: IntCmp.cmp,
          data: undefined
        }));

b("File \"bs_poly_mutable_set_test.res\", line 128, characters 4-11", Belt_MutableSet.eq(Belt_MutableSet.intersect(fromArray(Array_data_util.randomRange(21, 40)), fromArray(Array_data_util.randomRange(0, 20))), {
          cmp: IntCmp.cmp,
          data: undefined
        }));

b("File \"bs_poly_mutable_set_test.res\", line 131, characters 4-11", Belt_MutableSet.eq(Belt_MutableSet.intersect(fromArray([
                  1,
                  3,
                  4,
                  5,
                  7,
                  9
                ]), fromArray([
                  2,
                  4,
                  5,
                  6,
                  8,
                  10
                ])), fromArray([
              4,
              5
            ])));

b("File \"bs_poly_mutable_set_test.res\", line 132, characters 4-11", Belt_MutableSet.eq(Belt_MutableSet.diff(aa$2, bb$2), fromArray(Array_data_util.randomRange(0, 39))));

b("File \"bs_poly_mutable_set_test.res\", line 133, characters 4-11", Belt_MutableSet.eq(Belt_MutableSet.diff(bb$2, aa$2), fromArray(Array_data_util.randomRange(101, 120))));

b("File \"bs_poly_mutable_set_test.res\", line 135, characters 4-11", Belt_MutableSet.eq(Belt_MutableSet.diff(fromArray(Array_data_util.randomRange(21, 40)), fromArray(Array_data_util.randomRange(0, 20))), fromArray(Array_data_util.randomRange(21, 40))));

b("File \"bs_poly_mutable_set_test.res\", line 142, characters 4-11", Belt_MutableSet.eq(Belt_MutableSet.diff(fromArray(Array_data_util.randomRange(0, 20)), fromArray(Array_data_util.randomRange(21, 40))), fromArray(Array_data_util.randomRange(0, 20))));

b("File \"bs_poly_mutable_set_test.res\", line 150, characters 4-11", Belt_MutableSet.eq(Belt_MutableSet.diff(fromArray(Array_data_util.randomRange(0, 20)), fromArray(Array_data_util.randomRange(0, 40))), fromArray(Array_data_util.randomRange(0, -1))));

var a0 = fromArray(Array_data_util.randomRange(0, 1000));

var a1 = Belt_MutableSet.keep(a0, (function (x) {
        return x % 2 === 0;
      }));

var a2 = Belt_MutableSet.keep(a0, (function (x) {
        return x % 2 !== 0;
      }));

var match$4 = Belt_MutableSet.partition(a0, (function (x) {
        return x % 2 === 0;
      }));

var a4 = match$4[1];

var a3 = match$4[0];

b("File \"bs_poly_mutable_set_test.res\", line 162, characters 4-11", Belt_MutableSet.eq(a1, a3));

b("File \"bs_poly_mutable_set_test.res\", line 163, characters 4-11", Belt_MutableSet.eq(a2, a4));

Belt_List.forEach({
      hd: a0,
      tl: {
        hd: a1,
        tl: {
          hd: a2,
          tl: {
            hd: a3,
            tl: {
              hd: a4,
              tl: /* [] */0
            }
          }
        }
      }
    }, (function (x) {
        Belt_internalAVLset.checkInvariantInternal(x.data);
      }));

Mt.from_pair_suites("Bs_poly_mutable_set_test", suites.contents);

var N;

var I;

var A;

var L;

var $plus$plus = Belt_MutableSet.union;

var f = fromArray;

var $eq$tilde = Belt_MutableSet.eq;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.N = N;
exports.I = I;
exports.A = A;
exports.IntCmp = IntCmp;
exports.L = L;
exports.fromArray = fromArray;
exports.empty = empty;
exports.$plus$plus = $plus$plus;
exports.f = f;
exports.$eq$tilde = $eq$tilde;
/* IntCmp Not a pure module */

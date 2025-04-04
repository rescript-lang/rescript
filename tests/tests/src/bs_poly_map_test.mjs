// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";
import * as Belt_Id from "rescript/lib/es6/Belt_Id.js";
import * as Belt_Map from "rescript/lib/es6/Belt_Map.js";
import * as Belt_Set from "rescript/lib/es6/Belt_Set.js";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";
import * as Array_data_util from "./array_data_util.mjs";
import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, v) {
  Mt.bool_suites(test_id, suites, loc, v);
}

let Icmp = Belt_Id.comparable(Primitive_int.compare);

function mapOfArray(x) {
  return Belt_Map.fromArray(x, Icmp);
}

function setOfArray(x) {
  return Belt_Set.fromArray(x, Icmp);
}

function emptyMap() {
  return Belt_Map.make(Icmp);
}

function mergeInter(s1, s2) {
  return Belt_Set.fromArray(Belt_Map.keysToArray(Belt_Map.merge(s1, s2, (k, v1, v2) => {
    if (v1 !== undefined && v2 !== undefined) {
      return Primitive_option.some(undefined);
    }
    
  })), Icmp);
}

function mergeUnion(s1, s2) {
  return Belt_Set.fromArray(Belt_Map.keysToArray(Belt_Map.merge(s1, s2, (k, v1, v2) => {
    if (v1 !== undefined || v2 !== undefined) {
      return Primitive_option.some(undefined);
    }
    
  })), Icmp);
}

function mergeDiff(s1, s2) {
  return Belt_Set.fromArray(Belt_Map.keysToArray(Belt_Map.merge(s1, s2, (k, v1, v2) => {
    if (v1 !== undefined && v2 === undefined) {
      return Primitive_option.some(undefined);
    }
    
  })), Icmp);
}

function randomRange(i, j) {
  return Belt_Array.map(Array_data_util.randomRange(i, j), x => [
    x,
    x
  ]);
}

let u0 = Belt_Map.fromArray(randomRange(0, 100), Icmp);

let u1 = Belt_Map.fromArray(randomRange(30, 120), Icmp);

b("File \"bs_poly_map_test.res\", line 59, characters 4-11", Belt_Set.eq(mergeInter(u0, u1), Belt_Set.fromArray(Array_data_util.range(30, 100), Icmp)));

b("File \"bs_poly_map_test.res\", line 60, characters 4-11", Belt_Set.eq(mergeUnion(u0, u1), Belt_Set.fromArray(Array_data_util.range(0, 120), Icmp)));

b("File \"bs_poly_map_test.res\", line 61, characters 4-11", Belt_Set.eq(mergeDiff(u0, u1), Belt_Set.fromArray(Array_data_util.range(0, 29), Icmp)));

b("File \"bs_poly_map_test.res\", line 62, characters 4-11", Belt_Set.eq(mergeDiff(u1, u0), Belt_Set.fromArray(Array_data_util.range(101, 120), Icmp)));

let a0 = Belt_Map.fromArray(randomRange(0, 10), Icmp);

let a1 = Belt_Map.set(a0, 3, 33);

let a2 = Belt_Map.remove(a1, 3);

let a3 = Belt_Map.update(a2, 3, k => {
  if (k !== undefined) {
    return k + 1 | 0;
  } else {
    return 11;
  }
});

let a4 = Belt_Map.update(a2, 3, k => {
  if (k !== undefined) {
    return k + 1 | 0;
  }
  
});

let a5 = Belt_Map.remove(a0, 3);

let a6 = Belt_Map.remove(a5, 3);

b("File \"bs_poly_map_test.res\", line 83, characters 4-11", a5 === a6);

b("File \"bs_poly_map_test.res\", line 84, characters 4-11", Belt_Map.has(a0, 3));

b("File \"bs_poly_map_test.res\", line 85, characters 4-11", !Belt_Map.has(a5, 3));

b("File \"bs_poly_map_test.res\", line 86, characters 4-11", 3 === Belt_Map.getUndefined(a0, 3));

b("File \"bs_poly_map_test.res\", line 87, characters 4-11", 33 === Belt_Map.getUndefined(a1, 3));

b("File \"bs_poly_map_test.res\", line 88, characters 4-11", Belt_Map.getUndefined(a2, 3) === undefined);

b("File \"bs_poly_map_test.res\", line 90, characters 4-11", 11 === Belt_Map.getUndefined(a3, 3));

b("File \"bs_poly_map_test.res\", line 91, characters 4-11", Belt_Map.getUndefined(a4, 3) === undefined);

let a7 = Belt_Map.removeMany(a0, [
  7,
  8,
  0,
  1,
  3,
  2,
  4,
  922,
  4,
  5,
  6
]);

eq("File \"bs_poly_map_test.res\", line 94, characters 5-12", Belt_Map.keysToArray(a7), [
  9,
  10
]);

let a8 = Belt_Map.removeMany(a7, Array_data_util.randomRange(0, 100));

b("File \"bs_poly_map_test.res\", line 96, characters 4-11", Belt_Map.isEmpty(a8));

let u0$1 = Belt_Map.fromArray(randomRange(0, 100), Icmp);

let u1$1 = Belt_Map.set(u0$1, 3, 32);

eq("File \"bs_poly_map_test.res\", line 102, characters 5-12", Belt_Map.get(u1$1, 3), 32);

eq("File \"bs_poly_map_test.res\", line 103, characters 5-12", Belt_Map.get(u0$1, 3), 3);

function acc(m, is) {
  return Belt_Array.reduce(is, m, (a, i) => Belt_Map.update(a, i, n => {
    if (n !== undefined) {
      return n + 1 | 0;
    } else {
      return 1;
    }
  }));
}

let m = Belt_Map.make(Icmp);

let m1 = acc(m, Belt_Array.concat(Array_data_util.randomRange(0, 20), Array_data_util.randomRange(10, 30)));

b("File \"bs_poly_map_test.res\", line 120, characters 4-11", Belt_Map.eq(m1, Belt_Map.fromArray(Belt_Array.makeBy(31, i => [
  i,
  i >= 10 && i <= 20 ? 2 : 1
]), Icmp), (x, y) => x === y));

let v0 = Belt_Map.make(Icmp);

let v1 = Belt_Map.mergeMany(v0, Belt_Array.map(Array_data_util.randomRange(0, 10000), x => [
  x,
  x
]));

let v2 = Belt_Map.fromArray(Belt_Array.map(Array_data_util.randomRange(0, 10000), x => [
  x,
  x
]), Icmp);

b("File \"bs_poly_map_test.res\", line 144, characters 4-11", Belt_Map.eq(v1, v2, (x, y) => x === y));

function inc(x) {
  if (x !== undefined) {
    return x + 1 | 0;
  } else {
    return 0;
  }
}

let v3 = Belt_Map.update(v1, 10, inc);

let v4 = Belt_Map.update(v3, -10, inc);

let match = Belt_Map.split(v3, 5000);

let pres = match[1];

let match$1 = match[0];

let match$2 = Belt_Map.get(v3, 10);

b("File \"bs_poly_map_test.res\", line 155, characters 4-11", match$2 === 11);

let match$3 = Belt_Map.get(v3, -10);

b("File \"bs_poly_map_test.res\", line 162, characters 4-11", match$3 === undefined);

let match$4 = Belt_Map.get(v4, -10);

b("File \"bs_poly_map_test.res\", line 169, characters 4-11", match$4 === 0);

b("File \"bs_poly_map_test.res\", line 175, characters 4-11", Belt_Map.isEmpty(Belt_Map.remove(Belt_Map.make(Icmp), 0)));

b("File \"bs_poly_map_test.res\", line 176, characters 4-11", Belt_Map.isEmpty(Belt_Map.removeMany(Belt_Map.make(Icmp), [0])));

b("File \"bs_poly_map_test.res\", line 178, characters 4-11", pres === 5000);

b("File \"bs_poly_map_test.res\", line 184, characters 4-11", Belt_Array.eq(Belt_Map.keysToArray(match$1[0]), Belt_Array.makeBy(5000, i => i), (prim0, prim1) => prim0 === prim1));

b("File \"bs_poly_map_test.res\", line 185, characters 4-11", Belt_Array.eq(Belt_Map.keysToArray(match$1[1]), Belt_Array.makeBy(5000, i => 5001 + i | 0), (prim0, prim1) => prim0 === prim1));

let v7 = Belt_Map.remove(v3, 5000);

let match$5 = Belt_Map.split(v7, 5000);

let match$6 = match$5[0];

b("File \"bs_poly_map_test.res\", line 190, characters 4-11", match$5[1] === undefined);

b("File \"bs_poly_map_test.res\", line 196, characters 4-11", Belt_Array.eq(Belt_Map.keysToArray(match$6[0]), Belt_Array.makeBy(5000, i => i), (prim0, prim1) => prim0 === prim1));

b("File \"bs_poly_map_test.res\", line 197, characters 4-11", Belt_Array.eq(Belt_Map.keysToArray(match$6[1]), Belt_Array.makeBy(5000, i => 5001 + i | 0), (prim0, prim1) => prim0 === prim1));

Mt.from_pair_suites("Bs_poly_map_test", suites.contents);

let M;

let N;

let A;

let I;

export {
  suites,
  test_id,
  eq,
  b,
  Icmp,
  M,
  N,
  A,
  I,
  mapOfArray,
  setOfArray,
  emptyMap,
  mergeInter,
  mergeUnion,
  mergeDiff,
  randomRange,
  acc,
}
/* Icmp Not a pure module */

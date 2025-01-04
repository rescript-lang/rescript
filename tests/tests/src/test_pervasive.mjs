// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_List from "rescript/lib/es6/Belt_List.js";
import * as Pervasives from "rescript/lib/es6/Pervasives.js";

let Pervasives$1 = {
  length: Belt_List.length,
  size: Belt_List.size,
  head: Belt_List.head,
  headExn: Belt_List.headExn,
  tail: Belt_List.tail,
  tailExn: Belt_List.tailExn,
  add: Belt_List.add,
  get: Belt_List.get,
  getExn: Belt_List.getExn,
  make: Belt_List.make,
  makeByU: Belt_List.makeByU,
  makeBy: Belt_List.makeBy,
  shuffle: Belt_List.shuffle,
  drop: Belt_List.drop,
  take: Belt_List.take,
  splitAt: Belt_List.splitAt,
  concat: Belt_List.concat,
  concatMany: Belt_List.concatMany,
  reverseConcat: Belt_List.reverseConcat,
  flatten: Belt_List.flatten,
  mapU: Belt_List.mapU,
  map: Belt_List.map,
  zip: Belt_List.zip,
  zipByU: Belt_List.zipByU,
  zipBy: Belt_List.zipBy,
  mapWithIndexU: Belt_List.mapWithIndexU,
  mapWithIndex: Belt_List.mapWithIndex,
  fromArray: Belt_List.fromArray,
  toArray: Belt_List.toArray,
  reverse: Belt_List.reverse,
  mapReverseU: Belt_List.mapReverseU,
  mapReverse: Belt_List.mapReverse,
  forEachU: Belt_List.forEachU,
  forEach: Belt_List.forEach,
  forEachWithIndexU: Belt_List.forEachWithIndexU,
  forEachWithIndex: Belt_List.forEachWithIndex,
  reduceU: Belt_List.reduceU,
  reduce: Belt_List.reduce,
  reduceWithIndexU: Belt_List.reduceWithIndexU,
  reduceWithIndex: Belt_List.reduceWithIndex,
  reduceReverseU: Belt_List.reduceReverseU,
  reduceReverse: Belt_List.reduceReverse,
  mapReverse2U: Belt_List.mapReverse2U,
  mapReverse2: Belt_List.mapReverse2,
  forEach2U: Belt_List.forEach2U,
  forEach2: Belt_List.forEach2,
  reduce2U: Belt_List.reduce2U,
  reduce2: Belt_List.reduce2,
  reduceReverse2U: Belt_List.reduceReverse2U,
  reduceReverse2: Belt_List.reduceReverse2,
  everyU: Belt_List.everyU,
  every: Belt_List.every,
  someU: Belt_List.someU,
  some: Belt_List.some,
  every2U: Belt_List.every2U,
  every2: Belt_List.every2,
  some2U: Belt_List.some2U,
  some2: Belt_List.some2,
  cmpByLength: Belt_List.cmpByLength,
  cmpU: Belt_List.cmpU,
  cmp: Belt_List.cmp,
  eqU: Belt_List.eqU,
  eq: Belt_List.eq,
  hasU: Belt_List.hasU,
  has: Belt_List.has,
  getByU: Belt_List.getByU,
  getBy: Belt_List.getBy,
  keepU: Belt_List.keepU,
  keep: Belt_List.keep,
  filter: Belt_List.filter,
  keepWithIndexU: Belt_List.keepWithIndexU,
  keepWithIndex: Belt_List.keepWithIndex,
  filterWithIndex: Belt_List.filterWithIndex,
  keepMapU: Belt_List.keepMapU,
  keepMap: Belt_List.keepMap,
  partitionU: Belt_List.partitionU,
  partition: Belt_List.partition,
  unzip: Belt_List.unzip,
  getAssocU: Belt_List.getAssocU,
  getAssoc: Belt_List.getAssoc,
  hasAssocU: Belt_List.hasAssocU,
  hasAssoc: Belt_List.hasAssoc,
  removeAssocU: Belt_List.removeAssocU,
  removeAssoc: Belt_List.removeAssoc,
  setAssocU: Belt_List.setAssocU,
  setAssoc: Belt_List.setAssoc,
  sortU: Belt_List.sortU,
  sort: Belt_List.sort,
  failwith: Pervasives.failwith,
  invalid_arg: Pervasives.invalid_arg,
  Exit: Pervasives.Exit,
  abs: Pervasives.abs,
  lnot: Pervasives.lnot,
  max_int: Pervasives.max_int,
  min_int: Pervasives.min_int,
  infinity: Pervasives.infinity,
  neg_infinity: Pervasives.neg_infinity,
  max_float: Pervasives.max_float,
  min_float: Pervasives.min_float,
  epsilon_float: Pervasives.epsilon_float,
  classify_float: Pervasives.classify_float,
  char_of_int: Pervasives.char_of_int,
  string_of_bool: Pervasives.string_of_bool,
  bool_of_string: Pervasives.bool_of_string,
  bool_of_string_opt: Pervasives.bool_of_string_opt,
  int_of_string_opt: Pervasives.int_of_string_opt,
  $at: Pervasives.$at,
  panic: Pervasives.panic,
  assertEqual: Pervasives.assertEqual
};

function a0(prim) {
  return Math.abs(prim);
}

function a1(prim) {
  return Math.acos(prim);
}

function a2(prim) {
  return Math.tan(prim);
}

function a3(prim) {
  return Math.tanh(prim);
}

function a4(prim) {
  return Math.asin(prim);
}

function a5(prim0, prim1) {
  return Math.atan2(prim0, prim1);
}

function a6(prim) {
  return Math.atan(prim);
}

function a7(prim) {
  return Math.ceil(prim);
}

function a8(prim) {
  return Math.cos(prim);
}

function a9(prim) {
  return Math.cosh(prim);
}

function a10(prim) {
  return Math.exp(prim);
}

function a11(prim) {
  return Math.sin(prim);
}

function a12(prim) {
  return Math.sinh(prim);
}

function a13(prim) {
  return Math.sqrt(prim);
}

function a14(prim) {
  return Math.floor(prim);
}

function a15(prim) {
  return Math.log(prim);
}

function a16(prim) {
  return Math.log10(prim);
}

function a17(prim) {
  return Math.log1p(prim);
}

function a18(prim0, prim1) {
  return Math.pow(prim0, prim1);
}

let f = Pervasives.$at;

export {
  Pervasives$1 as Pervasives,
  f,
  a0,
  a1,
  a2,
  a3,
  a4,
  a5,
  a6,
  a7,
  a8,
  a9,
  a10,
  a11,
  a12,
  a13,
  a14,
  a15,
  a16,
  a17,
  a18,
}
/* No side effect */

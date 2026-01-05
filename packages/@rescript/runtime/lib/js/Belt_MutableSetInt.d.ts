import type * as rescript from "@rescript/runtime/types";
import type * as Belt_internalSetInt from "./Belt_internalSetInt.js";
import type * as Js from "./Js.js";

export type value = number;

export type t = rescript.opaque<
  "Belt_MutableSetInt.t",
  [],
  { data: Belt_internalSetInt.t; }
>;

export function make(): t;

export function fromArray(xs: value[]): t;

export function fromSortedArrayUnsafe(xs: value[]): t;

export function copy(d: t): t;

export function isEmpty(d: t): boolean;

export function has(d: t, x: value): boolean;

export function add(d: t, k: value): void;

export function addCheck(m: t, e: value): boolean;

export function mergeMany(d: t, arr: value[]): void;

export function remove(d: t, v: value): void;

export function removeCheck(d: t, v: value): boolean;

export function removeMany(d: t, xs: value[]): void;

export function union(dataa: t, datab: t): t;

export function intersect(dataa: t, datab: t): t;

export function diff(dataa: t, datab: t): t;

export function subset(a: t, b: t): boolean;

export function cmp(d0: t, d1: t): number;

export function eq(d0: t, d1: t): boolean;

export function forEachU(arg0: t, arg1: (arg0: value) => void): void;

export function forEach(d: t, f: (arg0: value) => void): void;

export function reduceU<A>(arg0: t, arg1: A, arg2: (arg0: A, arg1: value) => A): A;

export function reduce<A>(d: t, acc: A, cb: (arg0: A, arg1: value) => A): A;

export function everyU(arg0: t, arg1: (arg0: value) => boolean): boolean;

export function every(d: t, p: (arg0: value) => boolean): boolean;

export function someU(arg0: t, arg1: (arg0: value) => boolean): boolean;

export function some(d: t, p: (arg0: value) => boolean): boolean;

export function keepU(arg0: t, arg1: (arg0: value) => boolean): t;

export function keep(d: t, p: (arg0: value) => boolean): t;

export function partitionU(arg0: t, arg1: (arg0: value) => boolean): [t, t];

export function partition(d: t, p: (arg0: value) => boolean): [t, t];

export function size(d: t): number;

export function toList(d: t): rescript.list<value>;

export function toArray(d: t): value[];

export function minimum(d: t): rescript.option<value>;

export function minUndefined(d: t): Js.undefined_<value>;

export function maximum(d: t): rescript.option<value>;

export function maxUndefined(d: t): Js.undefined_<value>;

export function get(d: t, x: value): rescript.option<value>;

export function getUndefined(d: t, x: value): Js.undefined_<value>;

export function getExn(arg0: t, arg1: value): value;

export function getOrThrow(d: t, x: value): value;

export function split(d: t, key: value): [[t, t], boolean];

export function checkInvariantInternal(d: t): void;

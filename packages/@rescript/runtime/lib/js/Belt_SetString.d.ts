import type * as rescript from "@rescript/runtime/types";
import type * as Belt_internalSetString from "./Belt_internalSetString.js";
import type * as Js from "./Js.js";

export type value = string;

export type t = rescript.opaque<
  "Belt_SetString.t",
  [],
  Belt_internalSetString.t
>;

export const empty: t;

export function isEmpty(arg0: t): boolean;

export function minimum(arg0: t): rescript.option<value>;

export function minUndefined(arg0: t): Js.undefined_<value>;

export function maximum(arg0: t): rescript.option<value>;

export function maxUndefined(arg0: t): Js.undefined_<value>;

export function forEach(arg0: t, arg1: (arg0: value) => void): void;

export function reduce<A>(arg0: t, arg1: A, arg2: (arg0: A, arg1: value) => A): A;

export function every(arg0: t, arg1: (arg0: value) => boolean): boolean;

export function some(arg0: t, arg1: (arg0: value) => boolean): boolean;

export function keep(arg0: t, arg1: (arg0: value) => boolean): t;

export function partition(arg0: t, arg1: (arg0: value) => boolean): [t, t];

export function size(arg0: t): number;

export function toList(arg0: t): rescript.list<value>;

export function toArray(arg0: t): value[];

export function fromSortedArrayUnsafe(arg0: value[]): t;

export function checkInvariantInternal(arg0: t): void;

export function add(t: t, x: value): t;

export function mergeMany(h: t, arr: value[]): t;

export function remove(t: t, x: value): t;

export function removeMany(h: t, arr: value[]): t;

export function fromArray(arg0: value[]): t;

export function cmp(arg0: t, arg1: t): number;

export function eq(arg0: t, arg1: t): boolean;

export function get(arg0: t, arg1: value): rescript.option<value>;

export function getUndefined(arg0: t, arg1: value): Js.undefined_<value>;

export function getOrThrow(arg0: t, arg1: value): value;

export function getExn(arg0: t, arg1: value): value;

export function subset(arg0: t, arg1: t): boolean;

export function has(arg0: t, arg1: value): boolean;

export function split(t: t, x: value): [[t, t], boolean];

export function union(s1: t, s2: t): t;

export function intersect(s1: t, s2: t): t;

export function diff(s1: t, s2: t): t;

export function forEachU(arg0: t, arg1: (arg0: value) => void): void;

export function reduceU<A>(arg0: t, arg1: A, arg2: (arg0: A, arg1: value) => A): A;

export function everyU(arg0: t, arg1: (arg0: value) => boolean): boolean;

export function someU(arg0: t, arg1: (arg0: value) => boolean): boolean;

export function keepU(arg0: t, arg1: (arg0: value) => boolean): t;

export function partitionU(arg0: t, arg1: (arg0: value) => boolean): [t, t];

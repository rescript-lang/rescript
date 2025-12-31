import type * as rescript from "@rescript/runtime/types";
import type * as Belt_internalSetBuckets from "./Belt_internalSetBuckets.js";

export type key = string;

export type t = rescript.opaque<
  "Belt_HashSetString.t",
  [],
  Belt_internalSetBuckets.t<void, void, key>
>;

export function remove(h: t, key: key): void;

export function add(h: t, key: key): void;

export function has(h: t, key: key): boolean;

export function make(hintSize: number): t;

export function clear(arg0: t): void;

export function size(h: t): number;

export function forEach(arg0: t, arg1: (arg0: key) => void): void;

export function reduce<C>(arg0: t, arg1: C, arg2: (arg0: C, arg1: key) => C): C;

export function logStats(arg0: t): void;

export function toArray(arg0: t): key[];

export function copy(arg0: t): t;

export function getBucketHistogram(arg0: t): number[];

export function isEmpty(arg0: t): boolean;

export function fromArray(arr: key[]): t;

export function mergeMany(h: t, arr: key[]): void;

export function forEachU(arg0: t, arg1: (arg0: key) => void): void;

export function reduceU<C>(arg0: t, arg1: C, arg2: (arg0: C, arg1: key) => C): C;

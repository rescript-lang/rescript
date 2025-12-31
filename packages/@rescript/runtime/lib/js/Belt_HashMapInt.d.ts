import type * as rescript from "@rescript/runtime/types";
import type * as Belt_internalBuckets from "./Belt_internalBuckets.js";

export type key = number;

export type t<B> = rescript.opaque<
  "Belt_HashMapInt.t",
  [],
  Belt_internalBuckets.t<void, void, key, B>
>;

export function make<B>(hintSize: number): t<B>;

export function clear<B>(arg0: t<B>): void;

export function isEmpty<A>(arg0: t<A>): boolean;

export function set<A>(h: t<A>, key: key, value: A): void;

export function copy<A>(arg0: t<A>): t<A>;

export function get<A>(h: t<A>, key: key): rescript.option<A>;

export function has<B>(h: t<B>, key: key): boolean;

export function remove<A>(h: t<A>, key: key): void;

export function forEachU<B>(arg0: t<B>, arg1: (arg0: key, arg1: B) => void): void;

export function forEach<B>(arg0: t<B>, arg1: (arg0: key, arg1: B) => void): void;

export function reduceU<B, C>(
  arg0: t<B>,
  arg1: C,
  arg2: (arg0: C, arg1: key, arg2: B) => C,
): C;

export function reduce<B, C>(arg0: t<B>, arg1: C, arg2: (arg0: C, arg1: key, arg2: B) => C): C;

export function keepMapInPlaceU<A>(
  arg0: t<A>,
  arg1: (arg0: key, arg1: A) => rescript.option<A>,
): void;

export function keepMapInPlace<A>(
  arg0: t<A>,
  arg1: (arg0: key, arg1: A) => rescript.option<A>,
): void;

export function size<A>(h: t<A>): number;

export function toArray<A>(arg0: t<A>): [key, A][];

export function keysToArray<A>(arg0: t<A>): key[];

export function valuesToArray<A>(arg0: t<A>): A[];

export function fromArray<A>(arr: [key, A][]): t<A>;

export function mergeMany<A>(h: t<A>, arr: [key, A][]): void;

export function getBucketHistogram<A>(arg0: t<A>): number[];

export function logStats<A>(arg0: t<A>): void;

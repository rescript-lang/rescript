import type * as rescript from "@rescript/runtime/types";
import type * as Belt_internalSetBuckets from "./Belt_internalSetBuckets.js";
import type * as Belt_Id from "./Belt_Id.js";

export type t<A, Id> = rescript.opaque<
  "Belt_HashSet.t",
  [],
  Belt_internalSetBuckets.t<Belt_Id.hash<A, Id>, Belt_Id.eq<A, Id>, A>
>;

export type id<A, Id> = Belt_Id.hashable<A, Id>;

export function make<A, Id>(hintSize: number, id: id<A, Id>): t<A, Id>;

export function clear<A, Id>(arg0: t<A, Id>): void;

export function isEmpty<A, B>(arg0: t<A, B>): boolean;

export function add<A, Id>(h: t<A, Id>, key: A): void;

export function copy<A, Id>(arg0: t<A, Id>): t<A, Id>;

export function has<A, Id>(h: t<A, Id>, key: A): boolean;

export function remove<A, Id>(h: t<A, Id>, key: A): void;

export function forEachU<A, Id>(arg0: t<A, Id>, arg1: (arg0: A) => void): void;

export function forEach<A, Id>(arg0: t<A, Id>, arg1: (arg0: A) => void): void;

export function reduceU<A, Id, C>(arg0: t<A, Id>, arg1: C, arg2: (arg0: C, arg1: A) => C): C;

export function reduce<A, Id, C>(arg0: t<A, Id>, arg1: C, arg2: (arg0: C, arg1: A) => C): C;

export function size<A, Id>(h: t<A, Id>): number;

export function logStats<A, B>(arg0: t<A, B>): void;

export function toArray<A, Id>(arg0: t<A, Id>): A[];

export function fromArray<A, Id>(arr: A[], id: id<A, Id>): t<A, Id>;

export function mergeMany<A, Id>(h: t<A, Id>, arr: A[]): void;

export function getBucketHistogram<A, B>(arg0: t<A, B>): number[];

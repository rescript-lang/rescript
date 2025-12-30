import type * as rescript from "@rescript/runtime/types";
import type * as Belt_Id from "./Belt_Id.js";

export type t<Key, Value, Id> = rescript.opaque<"Belt_HashMap.t", [Key, Value, Id], Belt_internalBuckets.t<hash<A, Id>, eq<A, Id>, A, B>>;

export type id<A, Id> = Belt_Id.hashable<A, Id>;

export function clear<Key, Value, Id>(arg0: t<Key, Value, Id>): void;

export function size<A, B, C>(h: t<A, B, C>): number;

export function forEach<Key, Value, Id>(
  arg0: t<Key, Value, Id>,
  arg1: (arg0: Key, arg1: Value) => void,
): void;

export function reduce<Key, Value, Id, C>(
  arg0: t<Key, Value, Id>,
  arg1: C,
  arg2: (arg0: C, arg1: Key, arg2: Value) => C,
): C;

export function logStats<A, B, C>(arg0: t<A, B, C>): void;

export function keepMapInPlace<Key, Value, Id>(
  arg0: t<Key, Value, Id>,
  arg1: (arg0: Key, arg1: Value) => rescript.option<Value>,
): void;

export function toArray<Key, Value, Id>(arg0: t<Key, Value, Id>): [Key, Value][];

export function copy<Key, Value, Id>(arg0: t<Key, Value, Id>): t<Key, Value, Id>;

export function keysToArray<Key, A, B>(arg0: t<Key, A, B>): Key[];

export function valuesToArray<A, Value, B>(arg0: t<A, Value, B>): Value[];

export function getBucketHistogram<A, B, C>(arg0: t<A, B, C>): number[];

export function isEmpty<A, B, C>(arg0: t<A, B, C>): boolean;

export function set<Key, Value, Id>(h: t<Key, Value, Id>, key: Key, value: Value): void;

export function remove<Key, Value, Id>(h: t<Key, Value, Id>, key: Key): void;

export function get<Key, Value, Id>(h: t<Key, Value, Id>, key: Key): rescript.option<Value>;

export function has<Key, Value, Id>(h: t<Key, Value, Id>, key: Key): boolean;

export function make<Key, Id, Value>(hintSize: number, id: id<Key, Id>): t<Key, Value, Id>;

export function fromArray<Key, Value, Id>(
  arr: [Key, Value][],
  id: id<Key, Id>,
): t<Key, Value, Id>;

export function mergeMany<Key, Value, Id>(h: t<Key, Value, Id>, arr: [Key, Value][]): void;

export function forEachU<Key, Value, Id>(
  arg0: t<Key, Value, Id>,
  arg1: (arg0: Key, arg1: Value) => void,
): void;

export function reduceU<Key, Value, Id, C>(
  arg0: t<Key, Value, Id>,
  arg1: C,
  arg2: (arg0: C, arg1: Key, arg2: Value) => C,
): C;

export function keepMapInPlaceU<Key, Value, Id>(
  arg0: t<Key, Value, Id>,
  arg1: (arg0: Key, arg1: Value) => rescript.option<Value>,
): void;

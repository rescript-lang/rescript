import type * as rescript from "@rescript/runtime/types";
import type * as Belt_internalAVLtree from "./Belt_internalAVLtree.js";
import type * as Belt_Id from "./Belt_Id.js";
import type * as Js from "./Js.js";

export type t<Key, Value, Id> = rescript.opaque<
  "Belt_MapDict.t",
  [Id],
  Belt_internalAVLtree.t<Key, Value>
>;

export type cmp<Key, Id> = Belt_Id.cmp<Key, Id>;

export const empty: t<K, V, Id>;

export function isEmpty<K, V, Id>(arg0: t<K, V, Id>): boolean;

export function has<K, A, Id>(arg0: t<K, A, Id>, arg1: K, cmp: cmp<K, Id>): boolean;

export function cmpU<K, V, Id>(
  arg0: t<K, V, Id>,
  arg1: t<K, V, Id>,
  kcmp: cmp<K, Id>,
  vcmp: (arg0: V, arg1: V) => number,
): number;

export function cmp<K, V, Id>(
  arg0: t<K, V, Id>,
  arg1: t<K, V, Id>,
  kcmp: cmp<K, Id>,
  vcmp: (arg0: V, arg1: V) => number,
): number;

export function eqU<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: t<K, A, Id>,
  kcmp: cmp<K, Id>,
  veq: (arg0: A, arg1: A) => boolean,
): boolean;

export function eq<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: t<K, A, Id>,
  kcmp: cmp<K, Id>,
  veq: (arg0: A, arg1: A) => boolean,
): boolean;

export function findFirstByU<K, V, Id>(
  arg0: t<K, V, Id>,
  arg1: (arg0: K, arg1: V) => boolean,
): rescript.option<[K, V]>;

export function findFirstBy<K, V, Id>(
  arg0: t<K, V, Id>,
  arg1: (arg0: K, arg1: V) => boolean,
): rescript.option<[K, V]>;

export function forEachU<K, A, Id>(arg0: t<K, A, Id>, arg1: (arg0: K, arg1: A) => void): void;

export function forEach<K, A, Id>(arg0: t<K, A, Id>, arg1: (arg0: K, arg1: A) => void): void;

export function reduceU<K, A, Id, B>(
  arg0: t<K, A, Id>,
  arg1: B,
  arg2: (arg0: B, arg1: K, arg2: A) => B,
): B;

export function reduce<K, A, Id, B>(
  arg0: t<K, A, Id>,
  arg1: B,
  arg2: (arg0: B, arg1: K, arg2: A) => B,
): B;

export function everyU<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: (arg0: K, arg1: A) => boolean,
): boolean;

export function every<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: (arg0: K, arg1: A) => boolean,
): boolean;

export function someU<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: (arg0: K, arg1: A) => boolean,
): boolean;

export function some<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: (arg0: K, arg1: A) => boolean,
): boolean;

export function size<K, A, Id>(arg0: t<K, A, Id>): number;

export function toList<K, A, Id>(arg0: t<K, A, Id>): rescript.list<[K, A]>;

export function toArray<K, A, Id>(arg0: t<K, A, Id>): [K, A][];

export function fromArray<K, A, Id>(arg0: [K, A][], cmp: cmp<K, Id>): t<K, A, Id>;

export function keysToArray<K, A, Id>(arg0: t<K, A, Id>): K[];

export function valuesToArray<K, A, Id>(arg0: t<K, A, Id>): A[];

export function minKey<K, A, B>(arg0: t<K, A, B>): rescript.option<K>;

export function minKeyUndefined<K, A, B>(arg0: t<K, A, B>): Js.undefined_<K>;

export function maxKey<K, A, B>(arg0: t<K, A, B>): rescript.option<K>;

export function maxKeyUndefined<K, A, B>(arg0: t<K, A, B>): Js.undefined_<K>;

export function minimum<K, A>(arg0: t<K, A, A>): rescript.option<[K, A]>;

export function minUndefined<K, A>(arg0: t<K, A, A>): Js.undefined_<[K, A]>;

export function maximum<K, A>(arg0: t<K, A, A>): rescript.option<[K, A]>;

export function maxUndefined<K, A>(arg0: t<K, A, A>): Js.undefined_<[K, A]>;

export function get<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: K,
  cmp: cmp<K, Id>,
): rescript.option<A>;

export function getUndefined<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: K,
  cmp: cmp<K, Id>,
): Js.undefined_<A>;

export function getWithDefault<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: K,
  arg2: A,
  cmp: cmp<K, Id>,
): A;

export function getExn<K, A, Id>(arg0: t<K, A, Id>, arg1: K, cmp: cmp<K, Id>): A;

export function getOrThrow<K, A, Id>(arg0: t<K, A, Id>, arg1: K, cmp: cmp<K, Id>): A;

export function checkInvariantInternal<A, B, C>(arg0: t<A, B, C>): void;

export function remove<A, B, Id>(n: t<A, B, Id>, x: A, cmp: cmp<A, Id>): t<A, B, Id>;

export function removeMany<A, B, Id>(
  t: t<A, B, Id>,
  keys: A[],
  cmp: cmp<A, Id>,
): t<A, B, Id>;

export function set<A, B, Id>(
  t: t<A, B, Id>,
  newK: A,
  newD: B,
  cmp: cmp<A, Id>,
): t<A, B, Id>;

export function updateU<A, B, Id>(
  arg0: t<A, B, Id>,
  arg1: A,
  arg2: (arg0: rescript.option<B>) => rescript.option<B>,
  cmp: cmp<A, Id>,
): t<A, B, Id>;

export function update<A, B, Id>(
  t: t<A, B, Id>,
  newK: A,
  f: (arg0: rescript.option<B>) => rescript.option<B>,
  cmp: cmp<A, Id>,
): t<A, B, Id>;

export function mergeU<A, B, Id, C, D>(
  arg0: t<A, B, Id>,
  arg1: t<A, C, Id>,
  arg2: (arg0: A, arg1: rescript.option<B>, arg2: rescript.option<C>) => rescript.option<D>,
  cmp: cmp<A, Id>,
): t<A, D, Id>;

export function merge<A, B, Id, C, D>(
  s1: t<A, B, Id>,
  s2: t<A, C, Id>,
  f: (arg0: A, arg1: rescript.option<B>, arg2: rescript.option<C>) => rescript.option<D>,
  cmp: cmp<A, Id>,
): t<A, D, Id>;

export function mergeMany<A, B, Id>(
  h: t<A, B, Id>,
  arr: [A, B][],
  cmp: cmp<A, Id>,
): t<A, B, Id>;

export function keepU<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: (arg0: K, arg1: A) => boolean,
): t<K, A, Id>;

export function keep<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: (arg0: K, arg1: A) => boolean,
): t<K, A, Id>;

export function partitionU<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: (arg0: K, arg1: A) => boolean,
): [t<K, A, Id>, t<K, A, Id>];

export function partition<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: (arg0: K, arg1: A) => boolean,
): [t<K, A, Id>, t<K, A, Id>];

export function split<A, B, Id>(
  n: t<A, B, Id>,
  x: A,
  cmp: cmp<A, Id>,
): [[t<A, B, Id>, t<A, B, Id>], rescript.option<B>];

export function mapU<K, A, Id, B>(arg0: t<K, A, Id>, arg1: (arg0: A) => B): t<K, B, Id>;

export function map<K, A, Id, B>(arg0: t<K, A, Id>, arg1: (arg0: A) => B): t<K, B, Id>;

export function mapWithKeyU<K, A, Id, B>(
  arg0: t<K, A, Id>,
  arg1: (arg0: K, arg1: A) => B,
): t<K, B, Id>;

export function mapWithKey<K, A, Id, B>(
  arg0: t<K, A, Id>,
  arg1: (arg0: K, arg1: A) => B,
): t<K, B, Id>;

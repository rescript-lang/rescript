import type * as rescript from "@rescript/runtime/types";
import type * as Belt_Id from "./Belt_Id.js";
import type * as Js from "./Js.js";

export type t<K, V, Id> = rescript.opaque<"Belt_MutableMap.t", [K, V, Id]>;

export type id<Key, Id> = Belt_Id.comparable<Key, Id>;

export function remove<K, A, Id>(d: t<K, A, Id>, k: K): void;

export function removeMany<K, A, Id>(d: t<K, A, Id>, xs: K[]): void;

export function update<K, A, Id>(
  t: t<K, A, Id>,
  x: K,
  f: (arg0: rescript.option<A>) => rescript.option<A>,
): void;

export function make<K, Id, A>(id: id<K, Id>): t<K, A, Id>;

export function clear<A, B, C>(m: t<A, B, C>): void;

export function isEmpty<A, B, C>(d: t<A, B, C>): boolean;

export function minKey<K, A, B>(m: t<K, A, B>): rescript.option<K>;

export function minKeyUndefined<K, A, B>(m: t<K, A, B>): Js.undefined_<K>;

export function maxKey<K, A, B>(m: t<K, A, B>): rescript.option<K>;

export function maxKeyUndefined<K, A, B>(m: t<K, A, B>): Js.undefined_<K>;

export function minimum<K, A>(m: t<K, A, A>): rescript.option<[K, A]>;

export function minUndefined<K, A>(m: t<K, A, A>): Js.undefined_<[K, A]>;

export function maximum<K, A>(m: t<K, A, A>): rescript.option<[K, A]>;

export function maxUndefined<K, A>(m: t<K, A, A>): Js.undefined_<[K, A]>;

export function forEach<K, A, Id>(d: t<K, A, Id>, f: (arg0: K, arg1: A) => void): void;

export function reduce<K, A, Id, B>(d: t<K, A, Id>, acc: B, cb: (arg0: B, arg1: K, arg2: A) => B): B;

export function every<K, A, Id>(d: t<K, A, Id>, p: (arg0: K, arg1: A) => boolean): boolean;

export function some<K, A, Id>(d: t<K, A, Id>, p: (arg0: K, arg1: A) => boolean): boolean;

export function size<K, A, Id>(d: t<K, A, Id>): number;

export function toList<K, A, Id>(d: t<K, A, Id>): rescript.list<[K, A]>;

export function toArray<K, A, Id>(d: t<K, A, Id>): [K, A][];

export function keysToArray<K, A, B>(d: t<K, A, B>): K[];

export function valuesToArray<A, B>(d: t<A, A, B>): A[];

export function checkInvariantInternal<A, B, C>(d: t<A, B, C>): void;

export function cmp<K, A, Id>(
  m1: t<K, A, Id>,
  m2: t<K, A, Id>,
  cmp: (arg0: A, arg1: A) => number,
): number;

export function eq<K, A, Id>(
  m1: t<K, A, Id>,
  m2: t<K, A, Id>,
  cmp: (arg0: A, arg1: A) => boolean,
): boolean;

export function map<K, A, Id, B>(m: t<K, A, Id>, f: (arg0: A) => B): t<K, B, Id>;

export function mapWithKey<K, A, Id, B>(m: t<K, A, Id>, f: (arg0: K, arg1: A) => B): t<K, B, Id>;

export function get<K, A, Id>(m: t<K, A, Id>, x: K): rescript.option<A>;

export function getUndefined<K, A, Id>(m: t<K, A, Id>, x: K): Js.undefined_<A>;

export function getWithDefault<K, A, Id>(m: t<K, A, Id>, x: K, def: A): A;

export function getOrThrow<K, A, Id>(m: t<K, A, Id>, x: K): A;

export function getExn<K, A, Id>(arg0: t<K, A, Id>, arg1: K): A;

export function has<K, A, B>(m: t<K, A, B>, x: K): boolean;

export function fromArray<K, A, Id>(data: [K, A][], id: id<K, Id>): t<K, A, Id>;

export function set<K, A, Id>(m: t<K, A, Id>, e: K, v: A): void;

export function mergeMany<K, A, Id>(d: t<K, A, Id>, xs: [K, A][]): void;

export function cmpU<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: t<K, A, Id>,
  arg2: (arg0: A, arg1: A) => number,
): number;

export function eqU<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: t<K, A, Id>,
  arg2: (arg0: A, arg1: A) => boolean,
): boolean;

export function everyU<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: (arg0: K, arg1: A) => boolean,
): boolean;

export function forEachU<K, A, Id>(arg0: t<K, A, Id>, arg1: (arg0: K, arg1: A) => void): void;

export function mapU<K, A, Id, B>(arg0: t<K, A, Id>, arg1: (arg0: A) => B): t<K, B, Id>;

export function mapWithKeyU<K, A, Id, B>(
  arg0: t<K, A, Id>,
  arg1: (arg0: K, arg1: A) => B,
): t<K, B, Id>;

export function reduceU<K, A, Id, B>(
  arg0: t<K, A, Id>,
  arg1: B,
  arg2: (arg0: B, arg1: K, arg2: A) => B,
): B;

export function someU<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: (arg0: K, arg1: A) => boolean,
): boolean;

export function updateU<K, A, Id>(
  arg0: t<K, A, Id>,
  arg1: K,
  arg2: (arg0: rescript.option<A>) => rescript.option<A>,
): void;

import type * as rescript from "@rescript/runtime/types";
import type * as Belt_MapDict from "./Belt_MapDict.js";
import type * as Belt_Id from "./Belt_Id.js";
import type * as Js from "./Js.js";

export type t<Key, Value, Identity> = rescript.opaque<
  "Belt_Map.t",
  [Key, Value, Identity],
  {
    readonly cmp: cmp<K, Id>;
    readonly data: Belt_MapDict.t<K, V, Id>;
  }
>;

export type id<Key, Id> = Belt_Id.comparable<Key, Id>;

export function fromArray<K, V, Id>(data: [K, V][], id: id<K, Id>): t<K, V, Id>;

export function remove<K, V, Id>(m: t<K, V, Id>, x: K): t<K, V, Id>;

export function removeMany<K, V, Id>(m: t<K, V, Id>, x: K[]): t<K, V, Id>;

export function set<K, V, Id>(m: t<K, V, Id>, key: K, d: V): t<K, V, Id>;

export function mergeMany<K, V, Id>(m: t<K, V, Id>, e: [K, V][]): t<K, V, Id>;

export function update<K, V, Id>(
  m: t<K, V, Id>,
  key: K,
  f: (arg0: rescript.option<V>) => rescript.option<V>,
): t<K, V, Id>;

export function split<K, V, Id>(
  m: t<K, V, Id>,
  x: K,
): [[t<K, V, Id>, t<K, V, Id>], rescript.option<V>];

export function merge<K, V, Id, V2, V3>(
  s1: t<K, V, Id>,
  s2: t<K, V2, Id>,
  f: (arg0: K, arg1: rescript.option<V>, arg2: rescript.option<V2>) => rescript.option<V3>,
): t<K, V3, Id>;

export function make<K, Id, V>(id: id<K, Id>): t<K, V, Id>;

export function isEmpty<A, B, C>(map: t<A, B, C>): boolean;

export function findFirstBy<K, V, Id>(
  m: t<K, V, Id>,
  f: (arg0: K, arg1: V) => boolean,
): rescript.option<[K, V]>;

export function forEach<K, V, Id>(m: t<K, V, Id>, f: (arg0: K, arg1: V) => void): void;

export function reduce<K, V, Id, Acc>(
  m: t<K, V, Id>,
  acc: Acc,
  f: (arg0: Acc, arg1: K, arg2: V) => Acc,
): Acc;

export function every<K, V, Id>(m: t<K, V, Id>, f: (arg0: K, arg1: V) => boolean): boolean;

export function some<K, V, Id>(m: t<K, V, Id>, f: (arg0: K, arg1: V) => boolean): boolean;

export function keep<K, V, Id>(m: t<K, V, Id>, f: (arg0: K, arg1: V) => boolean): t<K, V, Id>;

export function partition<K, V, Id>(
  m: t<K, V, Id>,
  p: (arg0: K, arg1: V) => boolean,
): [t<K, V, Id>, t<K, V, Id>];

export function map<K, V, Id, V2>(m: t<K, V, Id>, f: (arg0: V) => V2): t<K, V2, Id>;

export function mapWithKey<K, V, Id, V2>(
  m: t<K, V, Id>,
  f: (arg0: K, arg1: V) => V2,
): t<K, V2, Id>;

export function size<K, V, Id>(map: t<K, V, Id>): number;

export function toList<K, V, Id>(map: t<K, V, Id>): rescript.list<[K, V]>;

export function toArray<K, V, Id>(m: t<K, V, Id>): [K, V][];

export function keysToArray<K, V, Id>(m: t<K, V, Id>): K[];

export function valuesToArray<K, V, Id>(m: t<K, V, Id>): V[];

export function minKey<K, A, B>(m: t<K, A, B>): rescript.option<K>;

export function minKeyUndefined<K, A, B>(m: t<K, A, B>): Js.undefined_<K>;

export function maxKey<K, A, B>(m: t<K, A, B>): rescript.option<K>;

export function maxKeyUndefined<K, A, B>(m: t<K, A, B>): Js.undefined_<K>;

export function minimum<K, V, A>(m: t<K, V, A>): rescript.option<[K, V]>;

export function minUndefined<K, V, A>(m: t<K, V, A>): Js.undefined_<[K, V]>;

export function maximum<K, V, A>(m: t<K, V, A>): rescript.option<[K, V]>;

export function maxUndefined<K, V, A>(m: t<K, V, A>): Js.undefined_<[K, V]>;

export function get<K, V, Id>(map: t<K, V, Id>, x: K): rescript.option<V>;

export function getUndefined<K, V, Id>(map: t<K, V, Id>, x: K): Js.undefined_<V>;

export function getWithDefault<K, V, Id>(map: t<K, V, Id>, x: K, def: V): V;

export function getOrThrow<K, V, Id>(map: t<K, V, Id>, x: K): V;

export function getExn<K, V, Id>(arg0: t<K, V, Id>, arg1: K): V;

export function has<K, V, Id>(map: t<K, V, Id>, x: K): boolean;

export function checkInvariantInternal<A, B, C>(m: t<A, B, C>): void;

export function eq<K, V, Id>(
  m1: t<K, V, Id>,
  m2: t<K, V, Id>,
  veq: (arg0: V, arg1: V) => boolean,
): boolean;

export function cmp<K, V, Id>(
  m1: t<K, V, Id>,
  m2: t<K, V, Id>,
  vcmp: (arg0: V, arg1: V) => number,
): number;

export function getData<K, V, Id>(m: t<K, V, Id>): Belt_MapDict.t<K, V, Id>;

export function getId<K, V, Id>(m: t<K, V, Id>): id<K, Id>;

export function packIdData<K, Id, V>(
  id: id<K, Id>,
  data: Belt_MapDict.t<K, V, Id>,
): t<K, V, Id>;

export function cmpU<K, V, Id>(
  arg0: t<K, V, Id>,
  arg1: t<K, V, Id>,
  arg2: (arg0: V, arg1: V) => number,
): number;

export function eqU<K, V, Id>(
  arg0: t<K, V, Id>,
  arg1: t<K, V, Id>,
  arg2: (arg0: V, arg1: V) => boolean,
): boolean;

export function everyU<K, V, Id>(
  arg0: t<K, V, Id>,
  arg1: (arg0: K, arg1: V) => boolean,
): boolean;

export function findFirstByU<K, V, Id>(
  arg0: t<K, V, Id>,
  arg1: (arg0: K, arg1: V) => boolean,
): rescript.option<[K, V]>;

export function forEachU<K, V, Id>(arg0: t<K, V, Id>, arg1: (arg0: K, arg1: V) => void): void;

export function keepU<K, V, Id>(
  arg0: t<K, V, Id>,
  arg1: (arg0: K, arg1: V) => boolean,
): t<K, V, Id>;

export function mapU<K, V, Id, V2>(arg0: t<K, V, Id>, arg1: (arg0: V) => V2): t<K, V2, Id>;

export function mapWithKeyU<K, V, Id, V2>(
  arg0: t<K, V, Id>,
  arg1: (arg0: K, arg1: V) => V2,
): t<K, V2, Id>;

export function mergeU<K, V, Id, V2, V3>(
  arg0: t<K, V, Id>,
  arg1: t<K, V2, Id>,
  arg2: (arg0: K, arg1: rescript.option<V>, arg2: rescript.option<V2>) => rescript.option<V3>,
): t<K, V3, Id>;

export function partitionU<K, V, Id>(
  arg0: t<K, V, Id>,
  arg1: (arg0: K, arg1: V) => boolean,
): [t<K, V, Id>, t<K, V, Id>];

export function reduceU<K, V, Id, Acc>(
  arg0: t<K, V, Id>,
  arg1: Acc,
  arg2: (arg0: Acc, arg1: K, arg2: V) => Acc,
): Acc;

export function someU<K, V, Id>(
  arg0: t<K, V, Id>,
  arg1: (arg0: K, arg1: V) => boolean,
): boolean;

export function updateU<K, V, Id>(
  arg0: t<K, V, Id>,
  arg1: K,
  arg2: (arg0: rescript.option<V>) => rescript.option<V>,
): t<K, V, Id>;

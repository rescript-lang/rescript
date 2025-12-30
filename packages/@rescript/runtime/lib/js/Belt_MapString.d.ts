import type * as rescript from "@rescript/runtime/types";
import type * as Js from "./Js.js";

export type key = string;

export type t<Value> = rescript.opaque<
  "Belt_MapString.t",
  [Value],
  Belt_internalAVLtree.t<key, A>
>;

export const empty: t<V>;

export function isEmpty<V>(arg0: t<V>): boolean;

export function minKey<A>(arg0: t<A>): rescript.option<key>;

export function minKeyUndefined<A>(arg0: t<A>): Js.undefined_<key>;

export function maxKey<A>(arg0: t<A>): rescript.option<key>;

export function maxKeyUndefined<A>(arg0: t<A>): Js.undefined_<key>;

export function minimum<V>(arg0: t<V>): rescript.option<[key, V]>;

export function minUndefined<V>(arg0: t<V>): Js.undefined_<[key, V]>;

export function maximum<V>(arg0: t<V>): rescript.option<[key, V]>;

export function maxUndefined<V>(arg0: t<V>): Js.undefined_<[key, V]>;

export function findFirstBy<V>(
  arg0: t<V>,
  arg1: (arg0: key, arg1: V) => boolean,
): rescript.option<[key, V]>;

export function forEach<V>(arg0: t<V>, arg1: (arg0: key, arg1: V) => void): void;

export function map<V, V2>(arg0: t<V>, arg1: (arg0: V) => V2): t<V2>;

export function mapWithKey<V, V2>(arg0: t<V>, arg1: (arg0: key, arg1: V) => V2): t<V2>;

export function reduce<V, V2>(
  arg0: t<V>,
  arg1: V2,
  arg2: (arg0: V2, arg1: key, arg2: V) => V2,
): V2;

export function every<V>(arg0: t<V>, arg1: (arg0: key, arg1: V) => boolean): boolean;

export function some<V>(arg0: t<V>, arg1: (arg0: key, arg1: V) => boolean): boolean;

export function keep<V>(arg0: t<V>, arg1: (arg0: key, arg1: V) => boolean): t<V>;

export function partition<V>(
  arg0: t<V>,
  arg1: (arg0: key, arg1: V) => boolean,
): [t<V>, t<V>];

export function size<V>(arg0: t<V>): number;

export function toList<V>(arg0: t<V>): rescript.list<[key, V]>;

export function toArray<V>(arg0: t<V>): [key, V][];

export function keysToArray<V>(arg0: t<V>): key[];

export function valuesToArray<V>(arg0: t<V>): V[];

export function checkInvariantInternal<A>(arg0: t<A>): void;

export function set<V>(t: t<V>, newK: key, newD: V): t<V>;

export function update<V>(
  t: t<V>,
  x: key,
  f: (arg0: rescript.option<V>) => rescript.option<V>,
): t<V>;

export function remove<V>(n: t<V>, x: key): t<V>;

export function removeMany<V>(t: t<V>, keys: key[]): t<V>;

export function mergeMany<V>(h: t<V>, arr: [key, V][]): t<V>;

export function has<V>(arg0: t<V>, arg1: key): boolean;

export function cmp<V>(
  arg0: t<V>,
  arg1: t<V>,
  arg2: (arg0: V, arg1: V) => number,
): number;

export function eq<V>(
  arg0: t<V>,
  arg1: t<V>,
  arg2: (arg0: V, arg1: V) => boolean,
): boolean;

export function get<V>(arg0: t<V>, arg1: key): rescript.option<V>;

export function getUndefined<V>(arg0: t<V>, arg1: key): Js.undefined_<V>;

export function getWithDefault<V>(arg0: t<V>, arg1: key, arg2: V): V;

export function getOrThrow<V>(arg0: t<V>, arg1: key): V;

export function getExn<V>(arg0: t<V>, arg1: key): V;

export function split<V>(arg0: key, arg1: t<V>): [t<V>, rescript.option<V>, t<V>];

export function merge<V, V2, C>(
  arg0: t<V>,
  arg1: t<V2>,
  arg2: (arg0: key, arg1: rescript.option<V>, arg2: rescript.option<V2>) => rescript.option<C>,
): t<C>;

export function fromArray<V>(arg0: [key, V][]): t<V>;

export function cmpU<V>(
  arg0: t<V>,
  arg1: t<V>,
  arg2: (arg0: V, arg1: V) => number,
): number;

export function eqU<V>(
  arg0: t<V>,
  arg1: t<V>,
  arg2: (arg0: V, arg1: V) => boolean,
): boolean;

export function everyU<V>(arg0: t<V>, arg1: (arg0: key, arg1: V) => boolean): boolean;

export function findFirstByU<V>(
  arg0: t<V>,
  arg1: (arg0: key, arg1: V) => boolean,
): rescript.option<[key, V]>;

export function forEachU<V>(arg0: t<V>, arg1: (arg0: key, arg1: V) => void): void;

export function keepU<V>(arg0: t<V>, arg1: (arg0: key, arg1: V) => boolean): t<V>;

export function mapU<V, V2>(arg0: t<V>, arg1: (arg0: V) => V2): t<V2>;

export function mapWithKeyU<V, V2>(arg0: t<V>, arg1: (arg0: key, arg1: V) => V2): t<V2>;

export function mergeU<V, V2, C>(
  arg0: t<V>,
  arg1: t<V2>,
  arg2: (arg0: key, arg1: rescript.option<V>, arg2: rescript.option<V2>) => rescript.option<C>,
): t<C>;

export function partitionU<V>(
  arg0: t<V>,
  arg1: (arg0: key, arg1: V) => boolean,
): [t<V>, t<V>];

export function reduceU<V, V2>(
  arg0: t<V>,
  arg1: V2,
  arg2: (arg0: V2, arg1: key, arg2: V) => V2,
): V2;

export function someU<V>(arg0: t<V>, arg1: (arg0: key, arg1: V) => boolean): boolean;

export function updateU<V>(
  arg0: t<V>,
  arg1: key,
  arg2: (arg0: rescript.option<V>) => rescript.option<V>,
): t<V>;

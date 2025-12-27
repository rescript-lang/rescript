import type * as rescript from "@rescript/runtime/types";
import type * as Js from "./Js.js";

export type key = number;

export type t<A> = rescript.opaque<"Belt_MutableMapInt.t", [A]>;

export function make<A>(): t<A>;

export function isEmpty<A>(m: t<A>): boolean;

export function clear<A>(m: t<A>): void;

export function minKeyUndefined<A>(m: t<A>): Js.undefined_<key>;

export function minKey<A>(m: t<A>): rescript.option<key>;

export function maxKeyUndefined<A>(m: t<A>): Js.undefined_<key>;

export function maxKey<A>(m: t<A>): rescript.option<key>;

export function minimum<A>(m: t<A>): rescript.option<[key, A]>;

export function minUndefined<A>(m: t<A>): Js.undefined_<[key, A]>;

export function maximum<A>(m: t<A>): rescript.option<[key, A]>;

export function maxUndefined<A>(m: t<A>): Js.undefined_<[key, A]>;

export function set<A>(m: t<A>, k: key, v: A): void;

export function forEach<A>(d: t<A>, f: (arg0: key, arg1: A) => void): void;

export function map<A, B>(d: t<A>, f: (arg0: A) => B): t<B>;

export function mapWithKey<A, B>(d: t<A>, f: (arg0: key, arg1: A) => B): t<B>;

export function reduce<A, B>(d: t<A>, acc: B, f: (arg0: B, arg1: key, arg2: A) => B): B;

export function every<A>(d: t<A>, f: (arg0: key, arg1: A) => boolean): boolean;

export function some<A>(d: t<A>, f: (arg0: key, arg1: A) => boolean): boolean;

export function size<A>(d: t<A>): number;

export function toList<A>(d: t<A>): rescript.list<[key, A]>;

export function toArray<A>(d: t<A>): [key, A][];

export function keysToArray<A>(d: t<A>): key[];

export function valuesToArray<A>(d: t<A>): A[];

export function checkInvariantInternal<A>(d: t<A>): void;

export function has<A>(d: t<A>, v: key): boolean;

export function remove<A>(d: t<A>, v: key): void;

export function update<A>(
  t: t<A>,
  x: key,
  f: (arg0: rescript.option<A>) => rescript.option<A>,
): void;

export function removeMany<A>(d: t<A>, xs: key[]): void;

export function fromArray<A>(xs: [key, A][]): t<A>;

export function cmp<A>(d0: t<A>, d1: t<A>, f: (arg0: A, arg1: A) => number): number;

export function eq<A>(d0: t<A>, d1: t<A>, f: (arg0: A, arg1: A) => boolean): boolean;

export function get<A>(d: t<A>, x: key): rescript.option<A>;

export function getUndefined<A>(d: t<A>, x: key): Js.undefined_<A>;

export function getWithDefault<A>(d: t<A>, x: key, def: A): A;

export function getOrThrow<A>(d: t<A>, x: key): A;

export function getExn<A>(arg0: t<A>, arg1: key): A;

export function cmpU<A>(
  arg0: t<A>,
  arg1: t<A>,
  arg2: (arg0: A, arg1: A) => number,
): number;

export function eqU<A>(
  arg0: t<A>,
  arg1: t<A>,
  arg2: (arg0: A, arg1: A) => boolean,
): boolean;

export function everyU<A>(arg0: t<A>, arg1: (arg0: key, arg1: A) => boolean): boolean;

export function forEachU<A>(arg0: t<A>, arg1: (arg0: key, arg1: A) => void): void;

export function mapU<A, B>(arg0: t<A>, arg1: (arg0: A) => B): t<B>;

export function mapWithKeyU<A, B>(arg0: t<A>, arg1: (arg0: key, arg1: A) => B): t<B>;

export function reduceU<A, B>(
  arg0: t<A>,
  arg1: B,
  arg2: (arg0: B, arg1: key, arg2: A) => B,
): B;

export function someU<A>(arg0: t<A>, arg1: (arg0: key, arg1: A) => boolean): boolean;

export function updateU<A>(
  arg0: t<A>,
  arg1: key,
  arg2: (arg0: rescript.option<A>) => rescript.option<A>,
): void;

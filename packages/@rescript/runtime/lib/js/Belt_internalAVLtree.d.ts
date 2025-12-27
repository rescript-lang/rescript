import type * as rescript from "@rescript/runtime/types";
import type * as Belt_Id from "./Belt_Id.js";
import type * as Js from "./Js.js";
import type * as Pervasives from "./Pervasives.js";

export type t<Key, A> = rescript.option<node<Key, A>>;

export interface node<K, V> {
  k: K;
  v: V;
  h: number;
  l: t<K, V>;
  r: t<K, V>;
}

export type cmp<K, Id> = Belt_Id.cmp<K, Id>;

export function copy<K, V>(n: t<K, V>): t<K, V>;

export function create<A, B>(l: t<A, B>, x: A, d: B, r: t<A, B>): t<A, B>;

export function singleton<A, B>(x: A, d: B): t<A, B>;

export function updateValue<K, V>(n: node<K, V>, newValue: V): node<K, V>;

export function bal<A, B>(l: t<A, B>, x: A, d: B, r: t<A, B>): t<A, B>;

export function minKey<A, B>(n: t<A, B>): rescript.option<A>;

export function minKeyUndefined<A, B>(n: t<A, B>): Js.undefined_<A>;

export function maxKey<A, B>(n: t<A, B>): rescript.option<A>;

export function maxKeyUndefined<A, B>(n: t<A, B>): Js.undefined_<A>;

export function minimum<A, B>(n: t<A, B>): rescript.option<[A, B]>;

export function minUndefined<A, B>(n: t<A, B>): Js.undefined_<[A, B]>;

export function maximum<A, B>(n: t<A, B>): rescript.option<[A, B]>;

export function maxUndefined<A, B>(n: t<A, B>): Js.undefined_<[A, B]>;

export function removeMinAuxWithRef<A, B>(
  n: node<A, B>,
  kr: Pervasives.ref<A>,
  vr: Pervasives.ref<B>,
): t<A, B>;

export function isEmpty<A, B>(x: t<A, B>): boolean;

export function stackAllLeft<A, B>(
  v: t<A, B>,
  s: rescript.list<node<A, B>>,
): rescript.list<node<A, B>>;

export function findFirstBy<A, B>(
  n: t<A, B>,
  p: (arg0: A, arg1: B) => boolean,
): rescript.option<[A, B]>;

export function forEach<A, B>(n: t<A, B>, f: (arg0: A, arg1: B) => void): void;

export function map<C, A, B>(n: t<C, A>, f: (arg0: A) => B): t<C, B>;

export function mapWithKey<A, B, C>(n: t<A, B>, f: (arg0: A, arg1: B) => C): t<A, C>;

export function reduce<A, B, C>(m: t<A, B>, accu: C, f: (arg0: C, arg1: A, arg2: B) => C): C;

export function every<A, B>(n: t<A, B>, p: (arg0: A, arg1: B) => boolean): boolean;

export function some<A, B>(n: t<A, B>, p: (arg0: A, arg1: B) => boolean): boolean;

export function join<A, B>(ln: t<A, B>, v: A, d: B, rn: t<A, B>): t<A, B>;

export function concat<A, B>(t1: t<A, B>, t2: t<A, B>): t<A, B>;

export function concatOrJoin<A, B>(
  t1: t<A, B>,
  v: A,
  d: rescript.option<B>,
  t2: t<A, B>,
): t<A, B>;

export function keepShared<A, B>(n: t<A, B>, p: (arg0: A, arg1: B) => boolean): t<A, B>;

export function keepMap<A, B, C>(
  n: t<A, B>,
  p: (arg0: A, arg1: B) => rescript.option<C>,
): t<A, C>;

export function partitionShared<A, B>(
  n: t<A, B>,
  p: (arg0: A, arg1: B) => boolean,
): [t<A, B>, t<A, B>];

export function lengthNode<A, B>(n: node<A, B>): number;

export function size<A, B>(n: t<A, B>): number;

export function toList<A, B>(s: t<A, B>): rescript.list<[A, B]>;

export function checkInvariantInternal<A, B>(v: t<A, B>): void;

export function fillArray<A, B>(n: node<A, B>, i: number, arr: [A, B][]): number;

export function toArray<A, B>(n: t<A, B>): [A, B][];

export function keysToArray<A, B>(n: t<A, B>): A[];

export function valuesToArray<A, B>(n: t<A, B>): B[];

export function fromSortedArrayRevAux<A, B>(
  arr: [A, B][],
  off: number,
  len: number,
): t<A, B>;

export function fromSortedArrayAux<A, B>(
  arr: [A, B][],
  off: number,
  len: number,
): t<A, B>;

export function fromSortedArrayUnsafe<A, B>(arr: [A, B][]): t<A, B>;

export function cmp<A, B, C>(
  s1: t<A, B>,
  s2: t<A, C>,
  kcmp: cmp<A, A>,
  vcmp: (arg0: B, arg1: C) => number,
): number;

export function eq<A, B, C>(
  s1: t<A, B>,
  s2: t<A, C>,
  kcmp: cmp<A, A>,
  veq: (arg0: B, arg1: C) => boolean,
): boolean;

export function get<A, B>(n: t<A, B>, x: A, cmp: cmp<A, A>): rescript.option<B>;

export function getUndefined<A, B>(
  n: t<A, B>,
  x: A,
  cmp: cmp<A, A>,
): Js.undefined_<B>;

export function getOrThrow<A, B>(n: t<A, B>, x: A, cmp: cmp<A, A>): B;

export function getWithDefault<A, B>(n: t<A, B>, x: A, def: B, cmp: cmp<A, A>): B;

export function has<A, B>(n: t<A, B>, x: A, cmp: cmp<A, A>): boolean;

export function balMutate<A, B>(nt: node<A, B>): node<A, B>;

export function updateMutate<A, B, Id>(
  t: t<A, B>,
  x: A,
  data: B,
  cmp: cmp<A, Id>,
): t<A, B>;

export function fromArray<A, B, Id>(xs: [A, B][], cmp: cmp<A, Id>): t<A, B>;

export function removeMinAuxWithRootMutate<A, B>(
  nt: node<A, B>,
  n: node<A, B>,
): t<A, B>;

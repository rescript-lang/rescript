import type * as rescript from "@rescript/runtime/types";
import type * as Belt_Id from "./Belt_Id.js";
import type * as Js from "./Js.js";
import type * as Pervasives from "./Pervasives.js";

export type t<Value> = rescript.option<node<Value>>;

export interface node<Value> {
  v: Value;
  h: number;
  l: t<Value>;
  r: t<Value>;
}

export type cmp<A, B> = Belt_Id.cmp<A, B>;

export function copy<A>(n: t<A>): t<A>;

export function create<A>(l: t<A>, v: A, r: t<A>): t<A>;

export function bal<A>(l: t<A>, v: A, r: t<A>): t<A>;

export function singleton<A>(x: A): t<A>;

export function minimum<A>(n: t<A>): rescript.option<A>;

export function minUndefined<A>(n: t<A>): Js.undefined_<A>;

export function maximum<A>(n: t<A>): rescript.option<A>;

export function maxUndefined<A>(n: t<A>): Js.undefined_<A>;

export function removeMinAuxWithRef<A>(n: node<A>, v: Pervasives.ref<A>): t<A>;

export function isEmpty<A>(n: t<A>): boolean;

export function stackAllLeft<A>(
  v: t<A>,
  s: rescript.list<node<A>>,
): rescript.list<node<A>>;

export function forEach<A>(n: t<A>, f: (arg0: A) => void): void;

export function reduce<A, B>(s: t<A>, accu: B, f: (arg0: B, arg1: A) => B): B;

export function every<A>(n: t<A>, p: (arg0: A) => boolean): boolean;

export function some<A>(n: t<A>, p: (arg0: A) => boolean): boolean;

export function joinShared<A>(ln: t<A>, v: A, rn: t<A>): t<A>;

export function concatShared<A>(t1: t<A>, t2: t<A>): t<A>;

export function keepShared<A>(n: t<A>, p: (arg0: A) => boolean): t<A>;

export function keepCopy<A>(n: t<A>, p: (arg0: A) => boolean): t<A>;

export function partitionShared<A>(
  n: t<A>,
  p: (arg0: A) => boolean,
): [t<A>, t<A>];

export function partitionCopy<A>(n: t<A>, p: (arg0: A) => boolean): [t<A>, t<A>];

export function lengthNode<A>(n: node<A>): number;

export function size<A>(n: t<A>): number;

export function toList<A>(s: t<A>): rescript.list<A>;

export function checkInvariantInternal<A>(v: t<A>): void;

export function fillArray<A>(n: node<A>, i: number, arr: A[]): number;

export function toArray<A>(n: t<A>): A[];

export function fromSortedArrayAux<A>(arr: A[], off: number, len: number): t<A>;

export function fromSortedArrayRevAux<A>(
  arr: A[],
  off: number,
  len: number,
): t<A>;

export function fromSortedArrayUnsafe<A>(arr: A[]): t<A>;

export function has<A, B>(t: t<A>, x: A, cmp: cmp<A, B>): boolean;

export function cmp<A, B>(s1: t<A>, s2: t<A>, cmp: cmp<A, B>): number;

export function eq<A, B>(s1: t<A>, s2: t<A>, cmp: cmp<A, B>): boolean;

export function subset<A, B>(s1: t<A>, s2: t<A>, cmp: cmp<A, B>): boolean;

export function get<A, B>(n: t<A>, x: A, cmp: cmp<A, B>): rescript.option<A>;

export function getUndefined<A, B>(n: t<A>, x: A, cmp: cmp<A, B>): Js.undefined_<A>;

export function getOrThrow<A, B>(n: t<A>, x: A, cmp: cmp<A, B>): A;

export function fromArray<A, B>(xs: A[], cmp: cmp<A, B>): t<A>;

export function addMutate<A, B>(cmp: cmp<A, B>, t: t<A>, x: A): t<A>;

export function balMutate<A>(nt: node<A>): node<A>;

export function removeMinAuxWithRootMutate<A>(nt: node<A>, n: node<A>): t<A>;

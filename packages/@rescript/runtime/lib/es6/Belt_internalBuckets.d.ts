import type * as rescript from "@rescript/runtime/types";
import type * as C from "./C.js";

export interface bucket<A, B> {
  key: A;
  value: B;
  next: C.opt<bucket<A, B>>;
}

export type t<Hash, Eq, A, B> = C.container<Hash, Eq, bucket<A, B>>;

export function copy<Hash, Eq, A, B>(x: t<Hash, Eq, A, B>): t<Hash, Eq, A, B>;

export function forEach<A, B, C>(h: t<A, B, A, B>, f: (arg0: A, arg1: B) => C): void;

export function reduce<A, B, C>(
  h: t<A, B, A, B>,
  init: C,
  f: (arg0: C, arg1: A, arg2: B) => C,
): C;

export function getBucketHistogram<A, B, C, D>(h: t<A, B, C, D>): number[];

export function logStats<A, B, C, D>(h: t<A, B, C, D>): void;

export function keepMapInPlace<A, B>(
  h: t<A, B, A, B>,
  f: (arg0: A, arg1: B) => rescript.option<B>,
): void;

export function fillArray<A, B>(
  i: number,
  arr: [A, B][],
  cell: bucket<A, B>,
): number;

export function keysToArray<A, B, C>(h: t<A, B, A, C>): A[];

export function valuesToArray<A, B, C>(h: t<A, B, C, B>): B[];

export function toArray<A, B>(h: t<A, B, A, B>): [A, B][];

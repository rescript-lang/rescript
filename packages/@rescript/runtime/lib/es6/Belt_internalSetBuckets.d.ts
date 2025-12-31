import type * as Belt_internalBucketsType from "./Belt_internalBucketsType.js";

export interface bucket<A> {
  key: A;
  next: Belt_internalBucketsType.opt<bucket<A>>;
}

export type t<Hash, Eq, A> = Belt_internalBucketsType.container<Hash, Eq, bucket<A>>;

export function copy<Hash, Eq, A>(x: t<Hash, Eq, A>): t<Hash, Eq, A>;

export function forEach<Hash, Eq, A>(h: t<Hash, Eq, A>, f: (arg0: A) => void): void;

export function fillArray<A>(i: number, arr: A[], cell: bucket<A>): number;

export function toArray<A, B>(h: t<A, B, A>): A[];

export function reduce<A, B>(h: t<A, B, A>, init: B, f: (arg0: B, arg1: A) => B): B;

export function logStats<A, B, C>(h: t<A, B, C>): void;

export function getBucketHistogram<A, B, C>(h: t<A, B, C>): number[];

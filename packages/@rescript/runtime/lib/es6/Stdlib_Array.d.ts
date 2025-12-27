import type * as rescript from "@rescript/runtime/types";
import type * as Stdlib_Ordering from "./Stdlib_Ordering.js";

export type t<A> = A[];

export type arrayLike<A> = rescript.opaque<"Stdlib_Array.arrayLike", [A]>;

export function make<A>(length: number, x: A): A[];

export function fromInitializer<A>(length: number, f: (arg0: number) => A): A[];

export function isEmpty<A>(arr: A[]): boolean;

export function equal<A>(a: A[], b: A[], eq: (arg0: A, arg1: A) => boolean): boolean;

export function compare<A>(
  a: A[],
  b: A[],
  cmp: (arg0: A, arg1: A) => Stdlib_Ordering.t,
): Stdlib_Ordering.t;

export function indexOfOpt<A>(arr: A[], item: A): rescript.option<number>;

export function lastIndexOfOpt<A>(arr: A[], item: A): rescript.option<number>;

export function reduce<A, B>(arr: A[], init: B, f: (arg0: B, arg1: A) => B): B;

export function reduceWithIndex<A, B>(
  arr: A[],
  init: B,
  f: (arg0: B, arg1: A, arg2: number) => B,
): B;

export function reduceRight<A, B>(arr: A[], init: B, f: (arg0: B, arg1: A) => B): B;

export function reduceRightWithIndex<A, B>(
  arr: A[],
  init: B,
  f: (arg0: B, arg1: A, arg2: number) => B,
): B;

export function findIndexOpt<A>(
  array: A[],
  finder: (arg0: A) => boolean,
): rescript.option<number>;

export function findLastIndexOpt<A>(
  array: A[],
  finder: (arg0: A) => boolean,
): rescript.option<number>;

export function shuffle<A>(xs: A[]): void;

export function toShuffled<A>(xs: A[]): A[];

export function filterMap<A, B>(a: A[], f: (arg0: A) => rescript.option<B>): B[];

export function keepSome<A>(__x: rescript.option<A>[]): A[];

export function filterMapWithIndex<A, B>(
  a: A[],
  f: (arg0: A, arg1: number) => rescript.option<B>,
): B[];

export function findMap<A, B>(
  arr: A[],
  f: (arg0: A) => rescript.option<B>,
): rescript.option<B>;

export function last<A>(a: A[]): rescript.option<A>;

import type * as rescript from "@rescript/runtime/types";

export type t<A> = A[];

export function get<A>(arr: t<A>, i: number): rescript.option<A>;

export function getExn<A>(arg0: t<A>, arg1: number): A;

export function getOrThrow<A>(arr: t<A>, i: number): A;

export function set<A>(arr: t<A>, i: number, v: A): boolean;

export function setExn<A>(arg0: t<A>, arg1: number, arg2: A): void;

export function setOrThrow<A>(arr: t<A>, i: number, v: A): void;

export function shuffleInPlace<A>(xs: t<A>): void;

export function shuffle<A>(xs: t<A>): t<A>;

export function reverseInPlace<A>(xs: t<A>): void;

export function reverse<A>(xs: t<A>): t<A>;

export function make<A>(l: number, f: A): t<A>;

export function range(start: number, finish: number): number[];

export function rangeBy(start: number, finish: number, step: number): number[];

export function makeByU<A>(arg0: number, arg1: (arg0: number) => A): t<A>;

export function makeBy<A>(l: number, f: (arg0: number) => A): t<A>;

export function makeByAndShuffleU<A>(arg0: number, arg1: (arg0: number) => A): t<A>;

export function makeByAndShuffle<A>(l: number, f: (arg0: number) => A): t<A>;

export function zip<A, B>(xs: t<A>, ys: B[]): [A, B][];

export function zipByU<A, B, C>(arg0: t<A>, arg1: B[], arg2: (arg0: A, arg1: B) => C): C[];

export function zipBy<A, B, C>(xs: t<A>, ys: B[], f: (arg0: A, arg1: B) => C): C[];

export function unzip<A, B>(a: [A, B][]): [t<A>, B[]];

export function concat<A>(a1: t<A>, a2: t<A>): t<A>;

export function concatMany<A>(arrs: t<A>[]): t<A>;

export function slice<A>(a: t<A>, offset: number, len: number): t<A>;

export function sliceToEnd<A>(a: t<A>, offset: number): t<A>;

export function fill<A>(a: t<A>, offset: number, len: number, v: A): void;

export function blit<A>(
  src: t<A>,
  srcOffset: number,
  dst: t<A>,
  dstOffset: number,
  len: number,
): void;

export function blitUnsafe<A>(
  src: t<A>,
  srcOffset: number,
  dst: t<A>,
  dstOffset: number,
  len: number,
): void;

export function forEachU<A>(arg0: t<A>, arg1: (arg0: A) => void): void;

export function forEach<A>(a: t<A>, f: (arg0: A) => void): void;

export function mapU<A, B>(arg0: t<A>, arg1: (arg0: A) => B): B[];

export function map<A, B>(a: t<A>, f: (arg0: A) => B): B[];

export function flatMapU<A, B>(arg0: t<A>, arg1: (arg0: A) => B[]): B[];

export function flatMap<A, B>(a: t<A>, f: (arg0: A) => B[]): B[];

export function getByU<A>(arg0: t<A>, arg1: (arg0: A) => boolean): rescript.option<A>;

export function getBy<A>(a: t<A>, p: (arg0: A) => boolean): rescript.option<A>;

export function getIndexByU<A>(
  arg0: t<A>,
  arg1: (arg0: A) => boolean,
): rescript.option<number>;

export function getIndexBy<A>(
  a: t<A>,
  p: (arg0: A) => boolean,
): rescript.option<number>;

export function keepU<A>(arg0: t<A>, arg1: (arg0: A) => boolean): t<A>;

export function keep<A>(a: t<A>, f: (arg0: A) => boolean): t<A>;

export function keepWithIndexU<A>(
  arg0: t<A>,
  arg1: (arg0: A, arg1: number) => boolean,
): t<A>;

export function keepWithIndex<A>(a: t<A>, f: (arg0: A, arg1: number) => boolean): t<A>;

export function keepMapU<A, B>(arg0: t<A>, arg1: (arg0: A) => rescript.option<B>): B[];

export function keepMap<A, B>(a: t<A>, f: (arg0: A) => rescript.option<B>): B[];

export function forEachWithIndexU<A>(
  arg0: t<A>,
  arg1: (arg0: number, arg1: A) => void,
): void;

export function forEachWithIndex<A>(a: t<A>, f: (arg0: number, arg1: A) => void): void;

export function mapWithIndexU<A, B>(arg0: t<A>, arg1: (arg0: number, arg1: A) => B): B[];

export function mapWithIndex<A, B>(a: t<A>, f: (arg0: number, arg1: A) => B): B[];

export function partitionU<A>(
  arg0: t<A>,
  arg1: (arg0: A) => boolean,
): [t<A>, t<A>];

export function partition<A>(a: t<A>, f: (arg0: A) => boolean): [t<A>, t<A>];

export function reduceU<B, A>(arg0: B[], arg1: A, arg2: (arg0: A, arg1: B) => A): A;

export function reduce<B, A>(a: B[], x: A, f: (arg0: A, arg1: B) => A): A;

export function reduceReverseU<B, A>(arg0: B[], arg1: A, arg2: (arg0: A, arg1: B) => A): A;

export function reduceReverse<B, A>(a: B[], x: A, f: (arg0: A, arg1: B) => A): A;

export function reduceReverse2U<A, B, C>(
  arg0: t<A>,
  arg1: B[],
  arg2: C,
  arg3: (arg0: C, arg1: A, arg2: B) => C,
): C;

export function reduceReverse2<A, B, C>(
  a: t<A>,
  b: B[],
  x: C,
  f: (arg0: C, arg1: A, arg2: B) => C,
): C;

export function reduceWithIndexU<A, B>(
  arg0: t<A>,
  arg1: B,
  arg2: (arg0: B, arg1: A, arg2: number) => B,
): B;

export function reduceWithIndex<A, B>(
  a: t<A>,
  x: B,
  f: (arg0: B, arg1: A, arg2: number) => B,
): B;

export function joinWithU<A>(
  arg0: t<A>,
  arg1: string,
  arg2: (arg0: A) => string,
): string;

export function joinWith<A>(
  a: t<A>,
  sep: string,
  toString: (arg0: A) => string,
): string;

export function someU<A>(arg0: t<A>, arg1: (arg0: A) => boolean): boolean;

export function some<A>(arr: t<A>, b: (arg0: A) => boolean): boolean;

export function everyU<A>(arg0: t<A>, arg1: (arg0: A) => boolean): boolean;

export function every<A>(arr: t<A>, b: (arg0: A) => boolean): boolean;

export function every2U<A, B>(
  arg0: t<A>,
  arg1: B[],
  arg2: (arg0: A, arg1: B) => boolean,
): boolean;

export function every2<A, B>(a: t<A>, b: B[], p: (arg0: A, arg1: B) => boolean): boolean;

export function some2U<A, B>(
  arg0: t<A>,
  arg1: B[],
  arg2: (arg0: A, arg1: B) => boolean,
): boolean;

export function some2<A, B>(a: t<A>, b: B[], p: (arg0: A, arg1: B) => boolean): boolean;

export function cmpU<A>(
  arg0: t<A>,
  arg1: t<A>,
  arg2: (arg0: A, arg1: A) => number,
): number;

export function cmp<A>(a: t<A>, b: t<A>, p: (arg0: A, arg1: A) => number): number;

export function eqU<A>(
  arg0: t<A>,
  arg1: t<A>,
  arg2: (arg0: A, arg1: A) => boolean,
): boolean;

export function eq<A>(a: t<A>, b: t<A>, p: (arg0: A, arg1: A) => boolean): boolean;

export function initU<A>(arg0: number, arg1: (arg0: number) => A): t<A>;

export function init<A>(n: number, f: (arg0: number) => A): t<A>;

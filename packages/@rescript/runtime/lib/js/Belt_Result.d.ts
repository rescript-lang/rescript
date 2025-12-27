import type * as rescript from "@rescript/runtime/types";

export type t<A, B> = rescript.result<A, B>;

export function getOrThrow<A, B>(x: t<A, B>): A;

export function getExn<A, B>(arg0: t<A, B>): A;

export function mapWithDefault<A, C, B>(opt: t<A, C>, default_: B, f: (arg0: A) => B): B;

export function map<A, C, B>(opt: t<A, C>, f: (arg0: A) => B): t<B, C>;

export function flatMap<A, C, B>(opt: t<A, C>, f: (arg0: A) => t<B, C>): t<B, C>;

export function getWithDefault<A, B>(opt: t<A, B>, default_: A): A;

export function isOk<A, B>(x: t<A, B>): boolean;

export function isError<A, B>(x: t<A, B>): boolean;

export function eq<A, C, B, D>(
  a: t<A, C>,
  b: t<B, D>,
  f: (arg0: A, arg1: B) => boolean,
): boolean;

export function cmp<A, C, B, D>(a: t<A, C>, b: t<B, D>, f: (arg0: A, arg1: B) => number): number;

export function cmpU<A, C, B, D>(
  arg0: t<A, C>,
  arg1: t<B, D>,
  arg2: (arg0: A, arg1: B) => number,
): number;

export function eqU<A, C, B, D>(
  arg0: t<A, C>,
  arg1: t<B, D>,
  arg2: (arg0: A, arg1: B) => boolean,
): boolean;

export function flatMapU<A, C, B>(arg0: t<A, C>, arg1: (arg0: A) => t<B, C>): t<B, C>;

export function mapU<A, C, B>(arg0: t<A, C>, arg1: (arg0: A) => B): t<B, C>;

export function mapWithDefaultU<A, C, B>(arg0: t<A, C>, arg1: B, arg2: (arg0: A) => B): B;

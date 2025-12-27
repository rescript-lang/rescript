import type * as rescript from "@rescript/runtime/types";

export type t<A> = rescript.option<A>;

export function some<A>(x: A): rescript.option<A>;

export function isSome<A>(x: rescript.option<A>): boolean;

export function isSomeValue<A>(
  eq: (arg0: A, arg1: A) => boolean,
  v: A,
  x: rescript.option<A>,
): boolean;

export function isNone<A>(x: rescript.option<A>): boolean;

export function getExn<A>(x: rescript.option<A>): A;

export function equal<A, B>(
  eq: (arg0: A, arg1: B) => boolean,
  a: rescript.option<A>,
  b: rescript.option<B>,
): boolean;

export function andThen<A, B>(
  f: (arg0: A) => rescript.option<B>,
  x: rescript.option<A>,
): rescript.option<B>;

export function map<A, B>(f: (arg0: A) => B, x: rescript.option<A>): rescript.option<B>;

export function getWithDefault<A>(a: A, x: rescript.option<A>): A;

export default getWithDefault;

export function filter<A>(
  f: (arg0: A) => boolean,
  x: rescript.option<A>,
): rescript.option<A>;

export function firstSome<A>(
  a: rescript.option<A>,
  b: rescript.option<A>,
): rescript.option<A>;

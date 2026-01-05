import type * as rescript from "@rescript/runtime/types";

export type t<A> = rescript.dict<A>;

export function $$delete<A>(dict: rescript.dict<A>, string_: string): void;

export function size<A>(arg0: rescript.dict<A>): number;

export function isEmpty<A>(arg0: rescript.dict<A>): boolean;

export function forEach<A>(arg0: rescript.dict<A>, arg1: (arg0: A) => void): void;

export function forEachWithKey<A>(
  arg0: rescript.dict<A>,
  arg1: (arg0: A, arg1: string) => void,
): void;

export function mapValues<A, B>(
  arg0: rescript.dict<A>,
  arg1: (arg0: A) => B,
): rescript.dict<B>;

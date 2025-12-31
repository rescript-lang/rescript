import type * as rescript from "@rescript/runtime/types";

export type t<A> = AsyncIterator<A>;

export interface value<A> {
  readonly done: boolean;
  readonly value: rescript.option<A>;
}

export function make<Value>(arg0: () => Promise<value<Value>>): t<Value>;

export function value<Value>(v: Value): value<Value>;

export function done<Value>(finalValue?: Value): value<Value>;

export function forEach<A>(
  iterator: t<A>,
  f: (arg0: rescript.option<A>) => void,
): Promise<void>;

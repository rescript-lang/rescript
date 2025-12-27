import type * as Js from "./Js.js";

export type opt<A> = Js.undefined_<A>;

export interface container<Hash, Eq, C> {
  size: number;
  buckets: opt<C>[];
  readonly hash: Hash;
  readonly eq: Eq;
}

export const emptyOpt: Js.undefined_<A>;

export function make<Hash, Eq, A>(
  hash: Hash,
  eq: Eq,
  hintSize: number,
): container<Hash, Eq, A>;

export function clear<A, B, C>(h: container<A, B, C>): void;

export function isEmpty<A, B, C>(h: container<A, B, C>): boolean;

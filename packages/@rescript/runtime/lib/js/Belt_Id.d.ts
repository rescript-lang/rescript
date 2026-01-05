import type * as rescript from "@rescript/runtime/types";

export type hash<A, Id> = rescript.opaque<
  "Belt_Id.hash",
  [Id],
  (arg0: A) => number
>;

export type eq<A, Id> = rescript.opaque<
  "Belt_Id.eq",
  [Id],
  (arg0: A, arg1: A) => boolean
>;

export type cmp<A, Id> = rescript.opaque<
  "Belt_Id.cmp",
  [Id],
  (arg0: A, arg1: A) => number
>;

export interface Comparable<Identity, T> {
  readonly cmp: cmp<T, Identity>;
}

export type comparable<Key, Id> = Comparable<Id, Key>;

export function MakeComparableU<Existential$Identity, T>(
  M: {
    readonly cmp: (arg0: T, arg1: T) => number;
  }
): Comparable<Existential$Identity, T>;

export function MakeComparable<Existential$Identity, T>(
  M: {
    readonly cmp: (arg0: T, arg1: T) => number;
  }
): Comparable<Existential$Identity, T>;

export function comparableU<Existential$Identity, A>(
  cmp: (arg0: A, arg1: A) => number,
): Comparable<Existential$Identity, A>;

export function comparable<Existential$Identity, A>(
  cmp: (arg0: A, arg1: A) => number,
): Comparable<Existential$Identity, A>;

export interface Hashable<Identity, T> {
  readonly hash: hash<T, Identity>;
  readonly eq: eq<T, Identity>;
}

export type hashable<Key, Id> = Hashable<Id, Key>;

export function MakeHashableU<Existential$Identity, T>(
  M: {
    readonly hash: (arg0: T) => number;
    readonly eq: (arg0: T, arg1: T) => boolean;
  }
): Hashable<Existential$Identity, T>;

export function MakeHashable<Existential$Identity, T>(
  M: {
    readonly hash: (arg0: T) => number;
    readonly eq: (arg0: T, arg1: T) => boolean;
  }
): Hashable<Existential$Identity, T>;

export function hashableU<Existential$Identity, A>(
  hash: (arg0: A) => number,
  eq: (arg0: A, arg1: A) => boolean,
): Hashable<Existential$Identity, A>;

export function hashable<Existential$Identity, A>(
  hash: (arg0: A) => number,
  eq: (arg0: A, arg1: A) => boolean,
): Hashable<Existential$Identity, A>;

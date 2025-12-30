import type * as rescript from "@rescript/runtime/types";
import type * as Belt_Id from "./Belt_Id.js";
import type * as Js from "./Js.js";

export type t<Value, Identity> = rescript.opaque<
  "Belt_SetDict.t",
  [Value, Identity],
  Belt_internalAVLset.t<K>
>;

export type cmp<Value, Id> = Belt_Id.cmp<Value, Id>;

export function add<Value, Id>(
  t: t<Value, Id>,
  x: Value,
  cmp: cmp<Value, Id>,
): t<Value, Id>;

export function remove<Value, Id>(
  t: t<Value, Id>,
  x: Value,
  cmp: cmp<Value, Id>,
): t<Value, Id>;

export function mergeMany<Value, Id>(
  h: t<Value, Id>,
  arr: Value[],
  cmp: cmp<Value, Id>,
): t<Value, Id>;

export function removeMany<Value, Id>(
  h: t<Value, Id>,
  arr: Value[],
  cmp: cmp<Value, Id>,
): t<Value, Id>;

export function split<Value, Id>(
  t: t<Value, Id>,
  x: Value,
  cmp: cmp<Value, Id>,
): [[t<Value, Id>, t<Value, Id>], boolean];

export function union<Value, Id>(
  s1: t<Value, Id>,
  s2: t<Value, Id>,
  cmp: cmp<Value, Id>,
): t<Value, Id>;

export function intersect<Value, Id>(
  s1: t<Value, Id>,
  s2: t<Value, Id>,
  cmp: cmp<Value, Id>,
): t<Value, Id>;

export function diff<Value, Id>(
  s1: t<Value, Id>,
  s2: t<Value, Id>,
  cmp: cmp<Value, Id>,
): t<Value, Id>;

export const empty: t<Value, Id>;

export function fromArray<Value, Id>(arg0: Value[], cmp: cmp<Value, Id>): t<Value, Id>;

export function isEmpty<A, B>(arg0: t<A, B>): boolean;

export function cmp<Value, Id>(
  arg0: t<Value, Id>,
  arg1: t<Value, Id>,
  cmp: cmp<Value, Id>,
): number;

export function eq<Value, Id>(
  arg0: t<Value, Id>,
  arg1: t<Value, Id>,
  cmp: cmp<Value, Id>,
): boolean;

export function has<Value, Id>(
  arg0: t<Value, Id>,
  arg1: Value,
  cmp: cmp<Value, Id>,
): boolean;

export function forEachU<Value, Id>(arg0: t<Value, Id>, arg1: (arg0: Value) => void): void;

export function forEach<Value, Id>(arg0: t<Value, Id>, arg1: (arg0: Value) => void): void;

export function reduceU<Value, Id, A>(
  arg0: t<Value, Id>,
  arg1: A,
  arg2: (arg0: A, arg1: Value) => A,
): A;

export function reduce<Value, Id, A>(
  arg0: t<Value, Id>,
  arg1: A,
  arg2: (arg0: A, arg1: Value) => A,
): A;

export function everyU<Value, Id>(
  arg0: t<Value, Id>,
  arg1: (arg0: Value) => boolean,
): boolean;

export function every<Value, Id>(
  arg0: t<Value, Id>,
  arg1: (arg0: Value) => boolean,
): boolean;

export function someU<Value, Id>(
  arg0: t<Value, Id>,
  arg1: (arg0: Value) => boolean,
): boolean;

export function some<Value, Id>(
  arg0: t<Value, Id>,
  arg1: (arg0: Value) => boolean,
): boolean;

export function size<Value, Id>(arg0: t<Value, Id>): number;

export function toList<Value, Id>(arg0: t<Value, Id>): rescript.list<Value>;

export function toArray<Value, Id>(arg0: t<Value, Id>): Value[];

export function minimum<Value, Id>(arg0: t<Value, Id>): rescript.option<Value>;

export function maximum<Value, Id>(arg0: t<Value, Id>): rescript.option<Value>;

export function maxUndefined<Value, Id>(arg0: t<Value, Id>): Js.undefined_<Value>;

export function minUndefined<Value, Id>(arg0: t<Value, Id>): Js.undefined_<Value>;

export function get<Value, Id>(
  arg0: t<Value, Id>,
  arg1: Value,
  cmp: cmp<Value, Id>,
): rescript.option<Value>;

export function getOrThrow<Value, Id>(
  arg0: t<Value, Id>,
  arg1: Value,
  cmp: cmp<Value, Id>,
): Value;

export function getExn<Value, Id>(
  arg0: t<Value, Id>,
  arg1: Value,
  cmp: cmp<Value, Id>,
): Value;

export function getUndefined<Value, Id>(
  arg0: t<Value, Id>,
  arg1: Value,
  cmp: cmp<Value, Id>,
): Js.undefined_<Value>;

export function fromSortedArrayUnsafe<Value, Id>(arg0: Value[]): t<Value, Id>;

export function subset<Value, Id>(
  arg0: t<Value, Id>,
  arg1: t<Value, Id>,
  cmp: cmp<Value, Id>,
): boolean;

export function keep<Value, Id>(
  arg0: t<Value, Id>,
  arg1: (arg0: Value) => boolean,
): t<Value, Id>;

export function keepU<Value, Id>(
  arg0: t<Value, Id>,
  arg1: (arg0: Value) => boolean,
): t<Value, Id>;

export function partitionU<Value, Id>(
  arg0: t<Value, Id>,
  arg1: (arg0: Value) => boolean,
): [t<Value, Id>, t<Value, Id>];

export function partition<Value, Id>(
  arg0: t<Value, Id>,
  arg1: (arg0: Value) => boolean,
): [t<Value, Id>, t<Value, Id>];

export function checkInvariantInternal<A, B>(arg0: t<A, B>): void;

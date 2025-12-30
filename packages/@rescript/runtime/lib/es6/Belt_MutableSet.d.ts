import type * as rescript from "@rescript/runtime/types";
import type * as Belt_Id from "./Belt_Id.js";
import type * as Js from "./Js.js";

export type t<Value, Identity> = rescript.opaque<
  "Belt_MutableSet.t",
  [Value, Identity],
  {
    readonly cmp: cmp<Value, Id>;
    data: Belt_internalAVLset.t<Value>;
  }
>;

export type id<Value, Id> = Belt_Id.comparable<Value, Id>;

export function remove<Value, Id>(d: t<Value, Id>, v: Value): void;

export function removeMany<Value, Id>(d: t<Value, Id>, xs: Value[]): void;

export function removeCheck<Value, Id>(d: t<Value, Id>, v: Value): boolean;

export function addCheck<Value, Id>(m: t<Value, Id>, e: Value): boolean;

export function add<Value, Id>(m: t<Value, Id>, e: Value): void;

export function mergeMany<Value, Id>(d: t<Value, Id>, xs: Value[]): void;

export function make<Value, Id>(id: id<Value, Id>): t<Value, Id>;

export function isEmpty<A, B>(d: t<A, B>): boolean;

export function minimum<Value, Id>(d: t<Value, Id>): rescript.option<Value>;

export function minUndefined<Value, Id>(d: t<Value, Id>): Js.undefined_<Value>;

export function maximum<Value, Id>(d: t<Value, Id>): rescript.option<Value>;

export function maxUndefined<Value, Id>(d: t<Value, Id>): Js.undefined_<Value>;

export function forEach<Value, Id>(d: t<Value, Id>, f: (arg0: Value) => void): void;

export function reduce<Value, Id, A>(d: t<Value, Id>, acc: A, cb: (arg0: A, arg1: Value) => A): A;

export function every<Value, Id>(d: t<Value, Id>, p: (arg0: Value) => boolean): boolean;

export function some<Value, Id>(d: t<Value, Id>, p: (arg0: Value) => boolean): boolean;

export function size<Value, Id>(d: t<Value, Id>): number;

export function toList<Value, Id>(d: t<Value, Id>): rescript.list<Value>;

export function toArray<Value, Id>(d: t<Value, Id>): Value[];

export function fromSortedArrayUnsafe<Value, Id>(
  xs: Value[],
  id: id<Value, Id>,
): t<Value, Id>;

export function checkInvariantInternal<A, B>(d: t<A, B>): void;

export function fromArray<Value, Id>(data: Value[], id: id<Value, Id>): t<Value, Id>;

export function cmp<Value, Id>(d0: t<Value, Id>, d1: t<Value, Id>): number;

export function eq<Value, Id>(d0: t<Value, Id>, d1: t<Value, Id>): boolean;

export function get<Value, Id>(d: t<Value, Id>, x: Value): rescript.option<Value>;

export function getUndefined<Value, Id>(d: t<Value, Id>, x: Value): Js.undefined_<Value>;

export function getOrThrow<Value, Id>(d: t<Value, Id>, x: Value): Value;

export function getExn<Value, Id>(arg0: t<Value, Id>, arg1: Value): Value;

export function split<Value, Id>(
  d: t<Value, Id>,
  key: Value,
): [[t<Value, Id>, t<Value, Id>], boolean];

export function keep<Value, Id>(d: t<Value, Id>, p: (arg0: Value) => boolean): t<Value, Id>;

export function partition<Value, Id>(
  d: t<Value, Id>,
  p: (arg0: Value) => boolean,
): [t<Value, Id>, t<Value, Id>];

export function subset<Value, Id>(a: t<Value, Id>, b: t<Value, Id>): boolean;

export function intersect<Value, Id>(a: t<Value, Id>, b: t<Value, Id>): t<Value, Id>;

export function diff<Value, Id>(a: t<Value, Id>, b: t<Value, Id>): t<Value, Id>;

export function union<Value, Id>(a: t<Value, Id>, b: t<Value, Id>): t<Value, Id>;

export function has<Value, Id>(d: t<Value, Id>, x: Value): boolean;

export function copy<Value, Id>(d: t<Value, Id>): t<Value, Id>;

export function everyU<Value, Id>(
  arg0: t<Value, Id>,
  arg1: (arg0: Value) => boolean,
): boolean;

export function forEachU<Value, Id>(arg0: t<Value, Id>, arg1: (arg0: Value) => void): void;

export function keepU<Value, Id>(
  arg0: t<Value, Id>,
  arg1: (arg0: Value) => boolean,
): t<Value, Id>;

export function partitionU<Value, Id>(
  arg0: t<Value, Id>,
  arg1: (arg0: Value) => boolean,
): [t<Value, Id>, t<Value, Id>];

export function reduceU<Value, Id, A>(
  arg0: t<Value, Id>,
  arg1: A,
  arg2: (arg0: A, arg1: Value) => A,
): A;

export function someU<Value, Id>(
  arg0: t<Value, Id>,
  arg1: (arg0: Value) => boolean,
): boolean;

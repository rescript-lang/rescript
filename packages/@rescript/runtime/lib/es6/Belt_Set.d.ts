import type * as rescript from "@rescript/runtime/types";
import type * as Belt_SetDict from "./Belt_SetDict.js";
import type * as Belt_Id from "./Belt_Id.js";
import type * as Js from "./Js.js";

export type t<Value, Identity> = rescript.opaque<"Belt_Set.t", [Value, Identity], {
  readonly cmp: cmp<Value, Id>;
  readonly data: Belt_SetDict.t<Value, Id>;
}>;

export type id<Value, Id> = Belt_Id.comparable<Value, Id>;

export function fromArray<Value, Id>(data: Value[], id: id<Value, Id>): t<Value, Id>;

export function remove<Value, Id>(m: t<Value, Id>, e: Value): t<Value, Id>;

export function add<Value, Id>(m: t<Value, Id>, e: Value): t<Value, Id>;

export function mergeMany<Value, Id>(m: t<Value, Id>, e: Value[]): t<Value, Id>;

export function removeMany<Value, Id>(m: t<Value, Id>, e: Value[]): t<Value, Id>;

export function union<Value, Id>(m: t<Value, Id>, n: t<Value, Id>): t<Value, Id>;

export function intersect<Value, Id>(m: t<Value, Id>, n: t<Value, Id>): t<Value, Id>;

export function diff<Value, Id>(m: t<Value, Id>, n: t<Value, Id>): t<Value, Id>;

export function subset<Value, Id>(m: t<Value, Id>, n: t<Value, Id>): boolean;

export function split<Value, Id>(
  m: t<Value, Id>,
  e: Value,
): [[t<Value, Id>, t<Value, Id>], boolean];

export function make<Value, Id>(id: id<Value, Id>): t<Value, Id>;

export function isEmpty<A, B>(m: t<A, B>): boolean;

export function cmp<Value, Id>(m: t<Value, Id>, n: t<Value, Id>): number;

export function eq<Value, Id>(m: t<Value, Id>, n: t<Value, Id>): boolean;

export function forEach<Value, Id>(m: t<Value, Id>, f: (arg0: Value) => void): void;

export function reduce<Value, Id, A>(m: t<Value, Id>, acc: A, f: (arg0: A, arg1: Value) => A): A;

export function every<Value, Id>(m: t<Value, Id>, f: (arg0: Value) => boolean): boolean;

export function some<Value, Id>(m: t<Value, Id>, f: (arg0: Value) => boolean): boolean;

export function keep<Value, Id>(m: t<Value, Id>, f: (arg0: Value) => boolean): t<Value, Id>;

export function partition<Value, Id>(
  m: t<Value, Id>,
  f: (arg0: Value) => boolean,
): [t<Value, Id>, t<Value, Id>];

export function size<Value, Id>(m: t<Value, Id>): number;

export function toList<Value, Id>(m: t<Value, Id>): rescript.list<Value>;

export function toArray<Value, Id>(m: t<Value, Id>): Value[];

export function minimum<Value, Id>(m: t<Value, Id>): rescript.option<Value>;

export function minUndefined<Value, Id>(m: t<Value, Id>): Js.undefined_<Value>;

export function maximum<Value, Id>(m: t<Value, Id>): rescript.option<Value>;

export function maxUndefined<Value, Id>(m: t<Value, Id>): Js.undefined_<Value>;

export function get<Value, Id>(m: t<Value, Id>, e: Value): rescript.option<Value>;

export function getUndefined<Value, Id>(m: t<Value, Id>, e: Value): Js.undefined_<Value>;

export function getOrThrow<Value, Id>(m: t<Value, Id>, e: Value): Value;

export function getExn<Value, Id>(arg0: t<Value, Id>, arg1: Value): Value;

export function has<Value, Id>(m: t<Value, Id>, e: Value): boolean;

export function fromSortedArrayUnsafe<Value, Id>(
  xs: Value[],
  id: id<Value, Id>,
): t<Value, Id>;

export function getData<Value, Id>(m: t<Value, Id>): Belt_SetDict.t<Value, Id>;

export function getId<Value, Id>(m: t<Value, Id>): id<Value, Id>;

export function packIdData<Value, Id>(
  id: id<Value, Id>,
  data: Belt_SetDict.t<Value, Id>,
): t<Value, Id>;

export function checkInvariantInternal<A, B>(d: t<A, B>): void;

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

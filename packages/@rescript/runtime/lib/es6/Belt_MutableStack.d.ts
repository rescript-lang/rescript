import type * as rescript from "@rescript/runtime/types";
import type * as Js from "./Js.js";

export type t<A> = rescript.opaque<
  "Belt_MutableStack.t",
  [A],
  { root: opt_cell<A>; }
>;

export function make<A>(): t<A>;

export function clear<A>(s: t<A>): void;

export function copy<A>(s: t<A>): t<A>;

export function push<A>(s: t<A>, x: A): void;

export function topUndefined<A>(s: t<A>): Js.undefined_<A>;

export function top<A>(s: t<A>): rescript.option<A>;

export function isEmpty<A>(s: t<A>): boolean;

export function popUndefined<A>(s: t<A>): Js.undefined_<A>;

export function pop<A>(s: t<A>): rescript.option<A>;

export function size<A>(s: t<A>): number;

export function forEach<A>(s: t<A>, f: (arg0: A) => void): void;

export function dynamicPopIter<A>(s: t<A>, f: (arg0: A) => void): void;

export function dynamicPopIterU<A>(arg0: t<A>, arg1: (arg0: A) => void): void;

export function forEachU<A>(arg0: t<A>, arg1: (arg0: A) => void): void;

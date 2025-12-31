import type * as rescript from "@rescript/runtime/types";
import type * as Js_undefined from "./Js_undefined.js";

export type t<T0> = rescript.opaque<
  "Js.t",
  [T0],
  { }
>;

export type null_<A> =
  | A
  | null;

export type undefined_<A> = Js_undefined.t<A>;

export type nullable<A> =
  | A
  | null
  | undefined;

export type null_undefined<A> = nullable<A>;

export function undefinedToOption<A>(arg0: undefined_<A>): rescript.option<A>;

export type promise<A, E> = rescript.opaque<
  "Js.promise",
  [A, E]
>;

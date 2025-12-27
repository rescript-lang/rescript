import type * as rescript from "@rescript/runtime/types";
import type * as Primitive_js_extern from "./Primitive_js_extern.js";
import type * as Primitive_object_extern from "./Primitive_object_extern.js";

export interface nested {
  readonly BS_PRIVATE_NESTED_SOME_NONE: number;
}

export type poly = rescript.opaque<"Primitive_option.poly", []>;

export function isNested(x: Primitive_object_extern.t): boolean;

export function some(x: Primitive_object_extern.t): Primitive_object_extern.t;

export function fromNullable<A>(
  x: Primitive_js_extern.null_undefined<A>,
): rescript.option<A>;

export function fromUndefined<A>(
  x: Primitive_js_extern.undefined_<A>,
): rescript.option<A>;

export function fromNull<A>(x: Primitive_js_extern.null_<A>): rescript.option<A>;

export function valFromOption(
  x: Primitive_object_extern.t,
): Primitive_object_extern.t;

export function toUndefined(
  x: rescript.option<Primitive_object_extern.t>,
): Primitive_js_extern.undefined_<Primitive_object_extern.t>;

export function unwrapPolyVar(x: rescript.option<poly>): Primitive_object_extern.t;

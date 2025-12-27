import type * as rescript from "@rescript/runtime/types";
import type * as Primitive_object from "./Primitive_object.js";

export type shape = rescript.opaque<"Primitive_module.shape", []>;

export function init(
  loc: [string, number, number],
  shape: shape,
): Primitive_object.t;

export function update(
  shape: shape,
  o: Primitive_object.t,
  n: Primitive_object.t,
): void;

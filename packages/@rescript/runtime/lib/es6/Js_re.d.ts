import type * as rescript from "@rescript/runtime/types";
import type * as Stdlib_RegExp from "./Stdlib_RegExp.js";

export type t = Stdlib_RegExp.t;

export type result = rescript.opaque<
  "Js_re.result",
  []
>;

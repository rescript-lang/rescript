import type * as rescript from "@rescript/runtime/types";

export type t<A> = rescript.opaque<"Stdlib_TypedArray.t", [A]>;

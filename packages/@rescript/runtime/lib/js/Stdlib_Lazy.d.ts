import type * as rescript from "@rescript/runtime/types";

export type t<A> = rescript.opaque<"Stdlib_Lazy.t", [A]>;

export function is_val<A>(l: t<A>): boolean;

export function force<A>(lzv: t<A>): A;

export function force_val<A>(lzv: t<A>): A;

export function from_fun<A>(closure: () => A): t<A>;

export function from_val<A>(value: A): t<A>;

export function make<A>(arg0: () => A): t<A>;

export function get<A>(arg0: t<A>): A;

export function isEvaluated<A>(arg0: t<A>): boolean;

import type * as rescript from "@rescript/runtime/types";

export type t<A> = Promise<A>;

export type error = rescript.opaque<"Js_promise2.error", []>;

export function then<A, B>(arg0: Promise<A>, arg1: (arg0: A) => Promise<B>): Promise<B>;

export function $$catch<A>(
  arg0: Promise<A>,
  arg1: (arg0: error) => Promise<A>,
): Promise<A>;

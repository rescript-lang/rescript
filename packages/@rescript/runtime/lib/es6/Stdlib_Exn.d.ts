import type * as rescript from "@rescript/runtime/types";

export type t = rescript.opaque<"Stdlib_Exn.t", [], unknown>;

export function asJsExn(exn: rescript.exn): rescript.option<t>;

export function raiseError<A>(str: string): A;

export function raiseEvalError<A>(str: string): A;

export function raiseRangeError<A>(str: string): A;

export function raiseReferenceError<A>(str: string): A;

export function raiseSyntaxError<A>(str: string): A;

export function raiseTypeError<A>(str: string): A;

export function raiseUriError<A>(str: string): A;

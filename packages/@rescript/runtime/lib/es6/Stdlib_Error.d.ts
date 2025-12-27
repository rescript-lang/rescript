import type * as rescript from "@rescript/runtime/types";
import type * as Stdlib_Exn from "./Stdlib_Exn.js";

export type t = Stdlib_Exn.t;

export type EvalError = {
};
export const $$EvalError: EvalError;

export type RangeError = {
};
export const $$RangeError: RangeError;

export type ReferenceError = {
};
export const $$ReferenceError: ReferenceError;

export type SyntaxError = {
};
export const $$SyntaxError: SyntaxError;

export type TypeError = {
};
export const $$TypeError: TypeError;

export type URIError = {
};
export const $$URIError: URIError;

export function fromException(exn: rescript.exn): rescript.option<t>;

export function panic<A>(msg: string): A;

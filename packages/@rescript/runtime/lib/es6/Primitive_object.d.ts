import type * as Primitive_object_extern from "./Primitive_object_extern.js";

export type t = Primitive_object_extern.t;

export function updateDummy(arg0: t, arg1: t): void;

export function compare(a: t, b: t): number;

export type eq = (arg0: t, arg1: t) => boolean;

export const equal: eq;

export const notequal: eq;

export const greaterequal: eq;

export const greaterthan: eq;

export const lessthan: eq;

export const lessequal: eq;

export function min(x: t, y: t): t;

export function max(x: t, y: t): t;

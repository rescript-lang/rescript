import type * as rescript from "@rescript/runtime/types";

export type t = boolean;

export function toString(b: boolean): string;

export function fromString(s: string): rescript.option<boolean>;

export function fromStringOrThrow(param: string): boolean;

export function fromStringExn(arg0: string): boolean;

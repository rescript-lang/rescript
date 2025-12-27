import type * as rescript from "@rescript/runtime/types";

export type t = bigint;

export function fromString(value: string): rescript.option<bigint>;

export function fromFloat(value: number): rescript.option<bigint>;

export function toInt(t: bigint): number;

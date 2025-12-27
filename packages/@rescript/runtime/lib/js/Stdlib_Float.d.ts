import type * as rescript from "@rescript/runtime/types";

export type t = number;

export type Constants = {
};
export const Constants: Constants;

export function fromString(i: string): rescript.option<number>;

export function clamp(
  min: number | undefined,
  max: number | undefined,
  value: number,
): number;

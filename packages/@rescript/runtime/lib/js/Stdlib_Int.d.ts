import type * as rescript from "@rescript/runtime/types";
import type * as Pervasives from "./Pervasives.js";

export type t = number;

export type Constants = {
  minValue: number;
  maxValue: number;
};
export const Constants: Constants;

export interface rangeOptions {
  readonly step?: rescript.option<number>;
  readonly inclusive?: rescript.option<boolean>;
}

export type Bitwise = {
  lnot: (arg0: number) => number;
};
export const Bitwise: Bitwise;

declare namespace Ref {
  type t = Pervasives.ref<number>;
}
export type Ref = {
};
export const Ref: Ref;

export function fromString(x: string, radix?: number): rescript.option<number>;

export function range(
  start: number,
  end: number,
  options?: rangeOptions,
): number[];

export function rangeWithOptions(
  start: number,
  end: number,
  options: rangeOptions,
): number[];

export function clamp(
  min: number | undefined,
  max: number | undefined,
  value: number,
): number;

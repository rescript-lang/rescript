import type * as rescript from "@rescript/runtime/types";
import type * as Stdlib_Ordering from "./Stdlib_Ordering.js";

export type t = Date;

export type msSinceEpoch = number;

export interface localeOptions {
  readonly dateStyle?: rescript.option<"full" | "long" | "medium" | "short">;
  readonly timeStyle?: rescript.option<"full" | "long" | "medium" | "short">;
  readonly weekday?: rescript.option<"narrow" | "long" | "short">;
  readonly era?: rescript.option<"narrow" | "long" | "short">;
  readonly year?: rescript.option<"numeric" | "2-digit">;
  readonly month?: rescript.option<"numeric" | "narrow" | "2-digit" | "long" | "short">;
  readonly day?: rescript.option<"numeric" | "2-digit">;
  readonly hour?: rescript.option<"numeric" | "2-digit">;
  readonly minute?: rescript.option<"numeric" | "2-digit">;
  readonly second?: rescript.option<"numeric" | "2-digit">;
  readonly timeZoneName?: rescript.option<"long" | "short">;
}

export type UTC = {
};
export const UTC: UTC;

export function equal(a: t, b: t): boolean;

export function compare(a: t, b: t): Stdlib_Ordering.t;

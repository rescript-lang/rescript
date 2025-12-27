import type * as rescript from "@rescript/runtime/types";

export type t = rescript.opaque<"Stdlib_Intl_NumberFormat_Grouping.t", []>;

export type parsed =
  | "min2"
  | "always"
  | "auto"
  | {
    readonly NAME: "bool";
    readonly VAL: boolean;
  };

export function parseJsValue<A>(
  value: A,
): rescript.option<"always" | "auto" | "min2" | {
  readonly NAME: "bool";
  readonly VAL: boolean;
}>;

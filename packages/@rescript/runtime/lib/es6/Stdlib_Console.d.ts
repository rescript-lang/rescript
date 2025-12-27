import type * as rescript from "@rescript/runtime/types";
import type * as Stdlib_Nullable from "./Stdlib_Nullable.js";

export interface dirOptions {
  readonly colors?: rescript.option<boolean>;
  readonly depth?: rescript.option<Stdlib_Nullable.t<number>>;
  readonly showHidden?: rescript.option<boolean>;
}

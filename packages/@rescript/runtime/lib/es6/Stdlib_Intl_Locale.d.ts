import type * as rescript from "@rescript/runtime/types";
import type * as Stdlib_Intl_Common from "./Stdlib_Intl_Common.js";

export type t = Intl.Locale;

export interface options {
  readonly baseName?: rescript.option<string>;
  readonly calendar?: rescript.option<Stdlib_Intl_Common.calendar>;
  readonly collation?: rescript.option<Stdlib_Intl_Common.collation>;
  readonly hourCycle?: rescript.option<"h24" | "h23" | "h12" | "h11">;
  readonly caseFirst?: rescript.option<"lower" | "false" | "upper">;
  readonly numberingSystem?: rescript.option<Stdlib_Intl_Common.numberingSystem>;
  readonly numeric?: rescript.option<boolean>;
  readonly language?: rescript.option<string>;
  readonly script?: rescript.option<string>;
  readonly region?: rescript.option<string>;
}

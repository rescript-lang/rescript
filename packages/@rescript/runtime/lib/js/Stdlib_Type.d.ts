import type * as rescript from "@rescript/runtime/types";
import type * as Stdlib_Symbol from "./Stdlib_Symbol.js";

export type t =
  | "undefined"
  | "symbol"
  | "bigint"
  | "object"
  | "function"
  | "string"
  | "number"
  | "boolean";

declare namespace Classify {
  type function_ = rescript.opaque<
  "Stdlib_Type.Classify.function_",
  []
>;
  type object_ = rescript.opaque<
  "Stdlib_Type.Classify.object_",
  []
>;
  type t =
    | {
      readonly TAG: "Bool";
      readonly _0: boolean;
    }
    | "Null"
    | "Undefined"
    | {
      readonly TAG: "String";
      readonly _0: string;
    }
    | {
      readonly TAG: "Number";
      readonly _0: number;
    }
    | {
      readonly TAG: "Object";
      readonly _0: Classify.object_;
    }
    | {
      readonly TAG: "Function";
      readonly _0: Classify.function_;
    }
    | {
      readonly TAG: "Symbol";
      readonly _0: Stdlib_Symbol.t;
    }
    | {
      readonly TAG: "BigInt";
      readonly _0: bigint;
    };
}
export type Classify = {
  classify: (arg0: A) => Classify.t;
};
export const Classify: Classify;

import type * as rescript from "@rescript/runtime/types";
import type * as Stdlib_Symbol from "./Stdlib_Symbol.js";
import type * as Stdlib_Type from "./Stdlib_Type.js";

export type symbol_ = Stdlib_Symbol.t;

export type obj_val = Stdlib_Type.Classify.object_;

export type undefined_val = rescript.opaque<
  "Js_types.undefined_val",
  []
>;

export type null_val = rescript.opaque<
  "Js_types.null_val",
  []
>;

export type function_val = Stdlib_Type.Classify.function_;

type t$bigint = t$BigInt;
type t$bool = t$Boolean;
type t$float = t$Number;
type t$function_val = t$Function;
type t$null_val = t$Null;
type t$obj_val = t$Object;
type t$string = t$String;
type t$symbol = t$Symbol;
type t$undefined_val = t$Undefined;
type t$Undefined = "Undefined";
type t$Null = "Null";
type t$Boolean = "Boolean";
type t$Number = "Number";
type t$String = "String";
type t$Function = "Function";
type t$Object = "Object";
type t$Symbol = "Symbol";
type t$BigInt = "BigInt";
export type t<_ extends undefined_val | null_val | boolean | number | string | function_val | obj_val | symbol | bigint = undefined_val | null_val | boolean | number | string | function_val | obj_val | symbol | bigint> =
  | t$Undefined
  | t$Null
  | t$Boolean
  | t$Number
  | t$String
  | t$Function
  | t$Object
  | t$Symbol
  | t$BigInt;

export function test<A>(x: A, v: t<undefined_val | null_val | boolean | number | string | function_val | obj_val | symbol | bigint>): boolean;

export type tagged_t =
  | "JSFalse"
  | "JSTrue"
  | "JSNull"
  | "JSUndefined"
  | {
    readonly TAG: "JSNumber";
    readonly _0: number;
  }
  | {
    readonly TAG: "JSString";
    readonly _0: string;
  }
  | {
    readonly TAG: "JSFunction";
    readonly _0: function_val;
  }
  | {
    readonly TAG: "JSObject";
    readonly _0: obj_val;
  }
  | {
    readonly TAG: "JSSymbol";
    readonly _0: symbol;
  }
  | {
    readonly TAG: "JSBigInt";
    readonly _0: bigint;
  };

export function classify<A>(x: A): tagged_t;

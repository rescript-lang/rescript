import type * as rescript from "@rescript/runtime/types";
import type * as Js_null from "./Js_null.js";
import type * as Js_string from "./Js_string.js";
import type * as Js_types from "./Js_types.js";

export type t =
  | boolean
  | null
  | string
  | number
  | rescript.dict<t>
  | t[];

declare namespace Kind {
  type json = t;
  type t$String = "String";
  type t$Number = "Number";
  type t$Object = "Object";
  type t$Array = "Array";
  type t$Boolean = "Boolean";
  type t$Null = "Null";
  type t<
    _ extends Js_string.t | number | rescript.dict<Kind.json> | Kind.json[] | boolean | Js_types.null_val = Js_string.t | number | rescript.dict<Kind.json> | Kind.json[] | boolean | Js_types.null_val
  > =
    | t$String
    | t$Number
    | t$Object
    | t$Array
    | t$Boolean
    | t$Null;
}
export type Kind = {
};
export const Kind: Kind;

export type tagged_t =
  | "JSONFalse"
  | "JSONTrue"
  | "JSONNull"
  | { readonly TAG: "JSONString"; readonly _0: string }
  | { readonly TAG: "JSONNumber"; readonly _0: number }
  | { readonly TAG: "JSONObject"; readonly _0: rescript.dict<t> }
  | { readonly TAG: "JSONArray"; readonly _0: t[] };

export function classify(x: t): tagged_t;

export function test<A>(x: A, v: Kind.t<Js_string.t | number | rescript.dict<Kind.json> | Kind.json[] | boolean | Js_types.null_val>): boolean;

export function decodeString(json: t): rescript.option<Js_string.t>;

export function decodeNumber(json: t): rescript.option<number>;

export function decodeObject(json: t): rescript.option<rescript.dict<t>>;

export function decodeArray(json: t): rescript.option<t[]>;

export function decodeBoolean(json: t): rescript.option<boolean>;

export function decodeNull(json: t): rescript.option<Js_null.t<Js_string.t | number | rescript.dict<Kind.json> | Kind.json[] | boolean | Js_types.null_val>>;

export function serializeExn<A>(
  x: A,
): string;

export function deserializeUnsafe<A>(s: string): A;

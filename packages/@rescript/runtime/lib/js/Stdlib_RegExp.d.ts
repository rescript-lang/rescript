import type * as rescript from "@rescript/runtime/types";

export type t = RegExp;

declare namespace Result {
  type t = rescript.option<string>[];
}
export type Result = {
};
export const Result: Result;

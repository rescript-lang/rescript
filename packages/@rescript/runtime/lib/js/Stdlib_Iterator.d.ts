import type * as rescript from "@rescript/runtime/types";

export type t<A> = Iterator<A>;

export interface value<A> {
  readonly done: boolean;
  readonly value: rescript.option<A>;
}

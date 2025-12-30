import type * as rescript from "@rescript/runtime/types";

export type t<A> = Promise<A>;

export interface promiseAndResolvers<A> {
  readonly promise: t<A>;
  readonly resolve: (arg0: A) => void;
  readonly reject: (arg0: rescript.exn) => void;
}

export type settledResult<A> =
  | {
    readonly status: "fulfilled";
    readonly value: A;
  }
  | {
    readonly status: "rejected";
    readonly reason: rescript.exn;
  };

export function $$catch<A>(promise: t<A>, callback: (arg0: rescript.exn) => t<A>): t<A>;

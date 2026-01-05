/**
 * TypeScript type definitions for ReScript's predefined/primitive types.
 *
 * These types represent the runtime representation of ReScript types in JavaScript/TypeScript.
 * They are used by the TypeScript output mode to generate accurate type annotations.
 */

export type list<T> =
  | { hd: T; tl: list<T> }
  | 0;

export type result<T, E> =
  | { TAG: "Ok"; _0: T }
  | { TAG: "Error"; _0: E };

export type ref<T> = {
  contents: T;
};

export type option<T> = T | undefined;

export type null_<T> = T | null;

export type nullable<T> = T | null | undefined;

export type dict<T> = Record<string, T>;

export type exn = Error & { RE_EXN_ID: string };

/** 
 * Utility type for FFIs
 */
export type external<Expected, T extends Expected, UseExternal extends boolean = false> = 
  UseExternal extends true
    ? T extends Expected ? Expected : never
    : T extends Expected ? T : never;

declare const $opaque: unique symbol;
/**
 * Utility type for opaque types
 */
export type opaque<
  Brand extends string, 
  PhantomParams extends readonly any[] = any[], 
  Underlying = {}
> = Underlying & {
  [$opaque]: { [K in Brand]: PhantomParams }
}

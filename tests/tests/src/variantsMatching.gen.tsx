/* TypeScript file generated from variantsMatching.res by genType. */

/* eslint-disable */
/* tslint:disable */

export type t = "thisIsA" | 42 | null | "D" | 3.14;

export type tNU = null | undefined;

export type MyUndefined_t<a> = undefined | a;

export type MyNull_t<a> = null | a;

export type MyNullable_t<a> = null | undefined | a;

export type MyNullableExtended_t<a> = null | undefined | "WhyNotAnotherOne" | a;

export type UntaggedWithBool_t = string | number | boolean | string;

export type UntaggedWithTuple_t = string | [number, number, string];

/* TypeScript file generated from ModuleAliases2.res by genType. */

/* eslint-disable */
/* tslint:disable */

export type record = { readonly x: number; readonly y: string };

export type Outer_outer = { readonly outer: string };

export type Outer_Inner_inner = { readonly inner: string };

export type OuterAlias_outer = Outer_outer;

export type OuterAlias_Inner_inner = Outer_Inner_inner;

export type InnerAlias_inner = OuterAlias_Inner_inner;

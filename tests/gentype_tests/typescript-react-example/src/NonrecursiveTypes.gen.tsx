/* TypeScript file generated from NonrecursiveTypes.res by genType. */

/* eslint-disable */
/* tslint:disable */

export type notRecursive = number;

export type M_notRecursive = notRecursive[];

export type M_recursive = { readonly self: M_recursive };

export type M_mutualRecursive = { readonly a: M_a };

export type M_a = { readonly self: M_mutualRecursive };

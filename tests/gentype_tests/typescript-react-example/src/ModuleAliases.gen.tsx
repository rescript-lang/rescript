/* TypeScript file generated from ModuleAliases.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as ModuleAliasesJS from './ModuleAliases.res.js';

export type Outer_Inner_innerT = { readonly inner: string };

export type Outer2_Inner2_InnerNested_t = { readonly nested: number };

export type Outer2_OuterInnerAlias_innerT = Outer_Inner_innerT;

export type Outer2_Inner2_OuterInnerAlias2_innerT = Outer2_OuterInnerAlias_innerT;

export type Outer2Alias_OuterInnerAlias_innerT = Outer2_OuterInnerAlias_innerT;

export type Outer2Alias_Inner2_OuterInnerAlias2_innerT = Outer2_Inner2_OuterInnerAlias2_innerT;

export type InnerNestedAlias_t = Outer2_Inner2_InnerNested_t;

export const testNested: (x:InnerNestedAlias_t) => InnerNestedAlias_t = ModuleAliasesJS.testNested as any;

export const testInner: (x:Outer2Alias_OuterInnerAlias_innerT) => Outer2Alias_OuterInnerAlias_innerT = ModuleAliasesJS.testInner as any;

export const testInner2: (x:Outer2Alias_Inner2_OuterInnerAlias2_innerT) => Outer2Alias_Inner2_OuterInnerAlias2_innerT = ModuleAliasesJS.testInner2 as any;

/* TypeScript file generated from NestedVariants.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as NestedVariantsJS from './NestedVariants.res.js';

export type typeL = 
    { TAG: "NonUnary"; _0: number; _1: number };

export type typeC = 
    { TAG: "C"; _0: string }
  | { TAG: "D"; _0: string };

export type typeB = { readonly c: typeC };

export type typeD = { TAG: "Int"; _0: number };

export type typeE = number;

export type typeA<a> = 
    { TAG: "A"; _0: a; _1: number }
  | { TAG: "B"; _0: a; _1: number };

export type typeF<a> = { TAG: "F"; _0: a } | { TAG: "G"; _0: a };

export type typeH = 
    { TAG: "H"; _0: typeD; _1: number }
  | { TAG: "I"; _0: typeD; _1: number };

export type typeJ = { TAG: "J"; _0: typeD; _1: typeD };

export type typeK = { TAG: "K"; _0: typeD; _1: typeD };

export type boxedBinary = 
    { TAG: "BB"; _0: typeD; _1: number }
  | { TAG: "Z"; _0: number };

export type unboxedBinary = { TAG: "UB"; _0: typeD; _1: number };

export type inline = 
    { TAG: "I"; readonly i: number; readonly j: number }
  | { TAG: "J"; readonly i: number; readonly j: number }
  | { TAG: "K"; _0: number; _1: number }
  | { TAG: "L"; _0: { readonly j: number; readonly i: number } };

export const makeVariant: () => typeL = NestedVariantsJS.makeVariant as any;

export const makeABC: () => typeA<typeB> = NestedVariantsJS.makeABC as any;

export const makeBC: () => typeB = NestedVariantsJS.makeBC as any;

export const makeAC: () => typeA<typeC> = NestedVariantsJS.makeAC as any;

export const makeAD: () => typeA<typeD> = NestedVariantsJS.makeAD as any;

export const makeAE: () => typeA<typeE> = NestedVariantsJS.makeAE as any;

export const makeFD: () => typeF<typeD> = NestedVariantsJS.makeFD as any;

export const makeHD: () => typeH = NestedVariantsJS.makeHD as any;

export const makeJ: () => typeJ = NestedVariantsJS.makeJ as any;

export const makeK: () => typeK = NestedVariantsJS.makeK as any;

export const testBoxedBinary: (param:boxedBinary) => number = NestedVariantsJS.testBoxedBinary as any;

export const testUnboxedBinary: (param:unboxedBinary) => number = NestedVariantsJS.testUnboxedBinary as any;

export const testInline: (x:inline) => inline = NestedVariantsJS.testInline as any;

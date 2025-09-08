/* TypeScript file generated from VariantsWithPayload.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as VariantsWithPayloadJS from './VariantsWithPayload.res.js';

export type payload = { readonly x: number; readonly y?: string };

export type withPayload = 
    "a"
  | "b"
  | "True"
  | "Twenty"
  | "Half"
  | { NAME: "c"; VAL: payload };

export type manyPayloads = 
    { NAME: "one"; VAL: number }
  | { NAME: "two"; VAL: [string, string] }
  | { NAME: "three"; VAL: payload };

export type simpleVariant = "A" | "B" | "C";

export type variantWithPayloads = 
    "A"
  | { TAG: "B"; _0: number }
  | { TAG: "C"; _0: number; _1: number }
  | { TAG: "D"; _0: number; _1: number }
  | { TAG: "E"; _0: number; _1: string; _2: number };

export type variant1Int = { TAG: "R"; _0: number };

export type variant1Object = { TAG: "R"; _0: payload };

export const testWithPayload: (x:withPayload) => withPayload = VariantsWithPayloadJS.testWithPayload as any;

export const printVariantWithPayload: (x:withPayload) => void = VariantsWithPayloadJS.printVariantWithPayload as any;

export const testManyPayloads: (x:manyPayloads) => manyPayloads = VariantsWithPayloadJS.testManyPayloads as any;

export const printManyPayloads: (x:manyPayloads) => void = VariantsWithPayloadJS.printManyPayloads as any;

export const testSimpleVariant: (x:simpleVariant) => simpleVariant = VariantsWithPayloadJS.testSimpleVariant as any;

export const testVariantWithPayloads: (x:variantWithPayloads) => variantWithPayloads = VariantsWithPayloadJS.testVariantWithPayloads as any;

export const printVariantWithPayloads: (x:variantWithPayloads) => void = VariantsWithPayloadJS.printVariantWithPayloads as any;

export const testVariant1Int: (x:variant1Int) => variant1Int = VariantsWithPayloadJS.testVariant1Int as any;

export const testVariant1Object: (x:variant1Object) => variant1Object = VariantsWithPayloadJS.testVariant1Object as any;

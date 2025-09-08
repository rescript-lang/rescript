/* TypeScript file generated from Variants.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as VariantsJS from './Variants.res.js';

import type {list} from '../src/shims/RescriptPervasives.shim';

export type weekday = 
    "monday"
  | "tuesday"
  | "wednesday"
  | "thursday"
  | "friday"
  | "saturday"
  | "sunday";

export type testGenTypeAs = "type_" | "module_" | "fortytwo";

export type testGenTypeAs2 = "type_" | "module" | 42;

export type testGenTypeAs3 = "type_" | "module" | 42;

export type x1 = "x" | "x1";

export type x2 = "x" | "x2";

export type type_ = "Type";
export type type = type_;

export type myList = "E" | { TAG: "C"; _0: number; _1: myList };

export type builtinList = list<number>;

export type result1<a,b> = 
    { TAG: "Ok"; _0: a }
  | { TAG: "Error"; _0: b };

export type result2<a,b> = 
    { TAG: "Ok"; _0: a }
  | { TAG: "Error"; _0: b };

export type result3<a,b> = 
    { TAG: "Ok"; _0: a }
  | { TAG: "Error"; _0: b };

export const isWeekend: (x:weekday) => boolean = VariantsJS.isWeekend as any;

export const monday: "monday" = VariantsJS.monday as any;

export const saturday: "saturday" = VariantsJS.saturday as any;

export const sunday: "sunday" = VariantsJS.sunday as any;

export const onlySunday: (param:"sunday") => void = VariantsJS.onlySunday as any;

export const swap: (x:"saturday" | "sunday") => "saturday" | "sunday" = VariantsJS.swap as any;

export const testConvert: (x:testGenTypeAs) => testGenTypeAs = VariantsJS.testConvert as any;

export const fortytwoOK: testGenTypeAs = VariantsJS.fortytwoOK as any;

export const fortytwoBAD: "fortytwo" = VariantsJS.fortytwoBAD as any;

export const testConvert2: (x:testGenTypeAs2) => testGenTypeAs2 = VariantsJS.testConvert2 as any;

export const testConvert3: (x:testGenTypeAs3) => testGenTypeAs3 = VariantsJS.testConvert3 as any;

export const testConvert2to3: (x:testGenTypeAs2) => testGenTypeAs3 = VariantsJS.testConvert2to3 as any;

export const id1: (x:x1) => x1 = VariantsJS.id1 as any;

export const id2: (x:x2) => x2 = VariantsJS.id2 as any;

export const polyWithOpt: (foo:string) => (undefined | (
    { NAME: "One"; VAL: string }
  | { NAME: "Two"; VAL: number })) = VariantsJS.polyWithOpt as any;

export const restResult1: (x:result1<number,string>) => result1<number,string> = VariantsJS.restResult1 as any;

export const restResult2: (x:result2<number,string>) => result2<number,string> = VariantsJS.restResult2 as any;

export const restResult3: (x:result3<number,string>) => result3<number,string> = VariantsJS.restResult3 as any;

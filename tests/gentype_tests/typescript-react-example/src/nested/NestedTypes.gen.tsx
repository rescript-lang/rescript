/* TypeScript file generated from NestedTypes.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as NestedTypesJS from './NestedTypes.res.js';

import type {Json_t as Js_Json_t} from '../../src/shims/Js.shim';

import type {List_t as Belt_List_t} from '../../src/shims/Belt.shim';

import type {M_t__ as TypeNameSanitize_M_t__} from '../../src/TypeNameSanitize.gen';

import type {list} from '../../src/shims/RescriptPervasives.shim';

import type {t_ as TypeNameSanitize_t_} from '../../src/TypeNameSanitize.gen';

import type {t as Location_t} from '../../src/location/location.gen';

export type t = number;

export type typeWithVars<x,y,z> = 
    { TAG: "A"; _0: x; _1: y }
  | { TAG: "B"; _0: z };

export type tree = {
  readonly label: string; 
  readonly left?: tree; 
  readonly right?: tree
};

export type selfRecursive = { readonly self: selfRecursive };

export type mutuallyRecursiveA = { readonly b: mutuallyRecursiveB };

export type mutuallyRecursiveB = { readonly a: mutuallyRecursiveA };

export abstract class opaqueVariant { protected opaque!: any }; /* simulate opaque types */

export type twice<a> = [a, a];

export type genTypeMispelled = number;

export type dictString = {[id: string]: string};

export type nullOrString = (null | string);

export type nullOrString2 = (null | string);

export type nullOrString3 = (null | string);

export type nullOrString4 = (null | string);

export type nullableOrString = (null | undefined | string);

export type nullableOrString2 = (null | undefined | string);

export type nullableOrString3 = (null | undefined | string);

export type nullableOrString4 = (null | undefined | string);

export type undefinedOrString = (undefined | string);

export type undefinedOrString2 = (undefined | string);

export type undefinedOrString3 = (undefined | string);

export type undefinedOrString4 = (undefined | string);

export type record = { readonly i: number; readonly s: string };

export type decorator<a,b> = (_1:a) => b;

export type marshalFields = {
  readonly _rec: string; 
  readonly _switch: string; 
  readonly switch: string; 
  readonly __: string; 
  readonly ___: string; 
  readonly foo__: string; 
  readonly _foo__: string; 
  readonly _Uppercase: string; 
  readonly _Uppercase__: string
};

export type marshalMutableField = { _match: number };

export type ocaml_array<a> = a[];

export type someRecord = { readonly id: number };

export type instantiateTypeParameter = ocaml_array<someRecord>;

export type vector<a> = [a, a];
export type Vector<a> = vector<a>;

export type date = Date;

export type ObjectId_t = number;

export type tPrimed = [TypeNameSanitize_t_, TypeNameSanitize_M_t__];

export const someIntList: list<number> = NestedTypesJS.someIntList as any;

export const map: <T1,T2>(_1:Belt_List_t<T1>, _2:((_1:T1) => T2)) => Belt_List_t<T2> = NestedTypesJS.map as any;

export const swap: (tree:tree) => tree = NestedTypesJS.swap as any;

export const selfRecursiveConverter: (param:selfRecursive) => selfRecursive = NestedTypesJS.selfRecursiveConverter as any;

export const mutuallyRecursiveConverter: (param:mutuallyRecursiveA) => mutuallyRecursiveB = NestedTypesJS.mutuallyRecursiveConverter as any;

export const testFunctionOnOptionsAsArgument: <T1,a>(a:(undefined | a), foo:((_1:(undefined | a)) => T1)) => T1 = NestedTypesJS.testFunctionOnOptionsAsArgument as any;

export const stringT: string = NestedTypesJS.stringT as any;

export const jsStringT: string = NestedTypesJS.jsStringT as any;

export const jsString2T: string = NestedTypesJS.jsString2T as any;

export const jsonStringify: (_1:Js_Json_t) => string = NestedTypesJS.jsonStringify as any;

export const testConvertNull: (x:(null | record)) => (null | record) = NestedTypesJS.testConvertNull as any;

export const testConvertLocation: (x:Location_t) => Location_t = NestedTypesJS.testConvertLocation as any;

export const testMarshalFields: marshalFields = NestedTypesJS.testMarshalFields as any;

export const setMatch: (x:marshalMutableField) => void = NestedTypesJS.setMatch as any;

export const testInstantiateTypeParameter: (x:instantiateTypeParameter) => instantiateTypeParameter = NestedTypesJS.testInstantiateTypeParameter as any;

export const currentTime: Date = NestedTypesJS.currentTime as any;

export const optFunction: (undefined | (() => number)) = NestedTypesJS.optFunction as any;

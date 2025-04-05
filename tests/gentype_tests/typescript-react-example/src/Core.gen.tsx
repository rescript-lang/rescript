/* TypeScript file generated from Core.res by genType. */

/* eslint-disable */
/* tslint:disable */

import {someFunWithNullThenOptionalArgs as someFunWithNullThenOptionalArgsNotChecked} from './CoreTS';

import {someFunWithNullUndefinedArg as someFunWithNullUndefinedArgNotChecked} from './CoreTS';

// In case of type error, check the type of 'someFunWithNullThenOptionalArgs' in 'Core.res' and './CoreTS'.
export const someFunWithNullThenOptionalArgsTypeChecked: (_1:(null | string), _2:(undefined | string)) => string = someFunWithNullThenOptionalArgsNotChecked as any;

// Export 'someFunWithNullThenOptionalArgs' early to allow circular import from the '.bs.js' file.
export const someFunWithNullThenOptionalArgs: unknown = someFunWithNullThenOptionalArgsTypeChecked as (_1:(null | string), _2:(undefined | string)) => string as any;

// In case of type error, check the type of 'someFunWithNullUndefinedArg' in 'Core.res' and './CoreTS'.
export const someFunWithNullUndefinedArgTypeChecked: (_1:(null | undefined | string), _2:number) => string = someFunWithNullUndefinedArgNotChecked as any;

// Export 'someFunWithNullUndefinedArg' early to allow circular import from the '.bs.js' file.
export const someFunWithNullUndefinedArg: unknown = someFunWithNullUndefinedArgTypeChecked as (_1:(null | undefined | string), _2:number) => string as any;

const CoreJS = require('./Core.res.js');

export type variant = "A" | { TAG: "B"; _0: string };

export type t1 = { readonly x?: string };

export type t2 = { readonly x: (undefined | string) };

export const null0: (x:(null | number)) => (null | number) = CoreJS.null0 as any;

export const null1: (x:(null | number)) => (null | number) = CoreJS.null1 as any;

export const nullable0: (x:(null | undefined | number)) => (null | undefined | number) = CoreJS.nullable0 as any;

export const nullable1: (x:(null | undefined | number)) => (null | undefined | number) = CoreJS.nullable1 as any;

export const undefined0: (x:(undefined | number)) => (undefined | number) = CoreJS.undefined0 as any;

export const undefined1: (x:(undefined | number)) => (undefined | number) = CoreJS.undefined1 as any;

export const dict0: (x:{[id: string]: string}) => {[id: string]: string} = CoreJS.dict0 as any;

export const dict1: (x:{[id: string]: string}) => {[id: string]: string} = CoreJS.dict1 as any;

export const promise0: (x:Promise<string>) => Promise<string> = CoreJS.promise0 as any;

export const promise1: (x:Promise<string>) => Promise<string> = CoreJS.promise1 as any;

export const date0: (x:Date) => Date = CoreJS.date0 as any;

export const date1: (x:Date) => Date = CoreJS.date1 as any;

export const bigint0: (x:bigint) => bigint = CoreJS.bigint0 as any;

export const regexp0: (x:RegExp) => RegExp = CoreJS.regexp0 as any;

export const regexp1: (x:RegExp) => RegExp = CoreJS.regexp1 as any;

export const map1: (x:Map<string,number>) => Map<string,number> = CoreJS.map1 as any;

export const weakmap1: (x:WeakMap<number[],number>) => WeakMap<number[],number> = CoreJS.weakmap1 as any;

export const set1: (x:Set<string>) => Set<string> = CoreJS.set1 as any;

export const weakset1: (x:WeakSet<number[]>) => WeakSet<number[]> = CoreJS.weakset1 as any;

export const option0: (x:(undefined | string)) => (undefined | string) = CoreJS.option0 as any;

export const option1: (x:(undefined | variant)) => (undefined | variant) = CoreJS.option1 as any;

export const jsonEncodeString1: unknown = CoreJS.jsonEncodeString1 as any;

export const jsonEncodeString2: unknown = CoreJS.jsonEncodeString2 as any;

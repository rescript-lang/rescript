/* TypeScript file generated from GenTypeImportExpectedErrors.res by genType. */

/* eslint-disable */
/* tslint:disable */

type $GenTypeImport<Expected, T extends Expected> = T;

import * as GenTypeImportExpectedErrorsJS from './GenTypeImportExpectedErrors.res.js';

import type {Type as Type$TypeScript} from 'external-module';

export type numberT = $GenTypeImport<number,Type$TypeScript>;

export type tupleT = $GenTypeImport<[number, string],Type$TypeScript>;

export type arrayT = $GenTypeImport<number[],Type$TypeScript>;

export type promiseT = $GenTypeImport<Promise<number>,Type$TypeScript>;

export type nestedArrayT = $GenTypeImport<Array<number[]>,Type$TypeScript>;

export type stringT = $GenTypeImport<string,Type$TypeScript>;

export const useNumber: (x:numberT) => numberT = GenTypeImportExpectedErrorsJS.useNumber as any;

export const useTuple: (x:tupleT) => tupleT = GenTypeImportExpectedErrorsJS.useTuple as any;

export const useArray: (x:arrayT) => arrayT = GenTypeImportExpectedErrorsJS.useArray as any;

export const usePromise: (x:promiseT) => promiseT = GenTypeImportExpectedErrorsJS.usePromise as any;

export const useNestedArray: (x:nestedArrayT) => nestedArrayT = GenTypeImportExpectedErrorsJS.useNestedArray as any;

export const useString: (x:stringT) => stringT = GenTypeImportExpectedErrorsJS.useString as any;

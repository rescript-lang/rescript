/* TypeScript file generated from GenTypeImportExpectedErrors.res by genType. */

/* eslint-disable */
/* tslint:disable */

type $GenTypeImport<Expected, T extends Expected> = T;

import * as GenTypeImportExpectedErrorsJS from './GenTypeImportExpectedErrors.res.js';

import type {Type as $$arrayT} from 'external-module';

import type {Type as $$nestedArrayT} from 'external-module';

import type {Type as $$numberT} from 'external-module';

import type {Type as $$promiseT} from 'external-module';

import type {Type as $$stringT} from 'external-module';

import type {Type as $$tupleT} from 'external-module';

export type numberT = $GenTypeImport<number,$$numberT>;

export type tupleT = $GenTypeImport<[number, string],$$tupleT>;

export type arrayT = $GenTypeImport<number[],$$arrayT>;

export type promiseT = $GenTypeImport<Promise<number>,$$promiseT>;

export type nestedArrayT = $GenTypeImport<Array<number[]>,$$nestedArrayT>;

export type stringT = $GenTypeImport<string,$$stringT>;

export const useNumber: (x:numberT) => numberT = GenTypeImportExpectedErrorsJS.useNumber as any;

export const useTuple: (x:tupleT) => tupleT = GenTypeImportExpectedErrorsJS.useTuple as any;

export const useArray: (x:arrayT) => arrayT = GenTypeImportExpectedErrorsJS.useArray as any;

export const usePromise: (x:promiseT) => promiseT = GenTypeImportExpectedErrorsJS.usePromise as any;

export const useNestedArray: (x:nestedArrayT) => nestedArrayT = GenTypeImportExpectedErrorsJS.useNestedArray as any;

export const useString: (x:stringT) => stringT = GenTypeImportExpectedErrorsJS.useString as any;

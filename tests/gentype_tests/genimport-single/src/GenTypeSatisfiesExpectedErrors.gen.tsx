/* TypeScript file generated from GenTypeSatisfiesExpectedErrors.res by genType. */

/* eslint-disable */
/* tslint:disable */

export type $RescriptTypeSatisfiesTypeScriptType<
RescriptType,
TypeScriptType extends RescriptType
> = TypeScriptType;

import * as GenTypeSatisfiesExpectedErrorsJS from './GenTypeSatisfiesExpectedErrors.res.js';

export type numberT = $RescriptTypeSatisfiesTypeScriptType<
  number,
  import("external-module").Type
>;

export type tupleT = $RescriptTypeSatisfiesTypeScriptType<
  [number, string],
  import("external-module").Type
>;

export type arrayT = $RescriptTypeSatisfiesTypeScriptType<
  number[],
  import("external-module").Type
>;

export type promiseT = $RescriptTypeSatisfiesTypeScriptType<
  Promise<number>,
  import("external-module").Type
>;

export type nestedArrayT = $RescriptTypeSatisfiesTypeScriptType<
  Array<number[]>,
  import("external-module").Type
>;

export type stringT = $RescriptTypeSatisfiesTypeScriptType<
  string,
  import("external-module").Type
>;

export const useNumber: (x:numberT) => numberT = GenTypeSatisfiesExpectedErrorsJS.useNumber as any;

export const useTuple: (x:tupleT) => tupleT = GenTypeSatisfiesExpectedErrorsJS.useTuple as any;

export const useArray: (x:arrayT) => arrayT = GenTypeSatisfiesExpectedErrorsJS.useArray as any;

export const usePromise: (x:promiseT) => promiseT = GenTypeSatisfiesExpectedErrorsJS.usePromise as any;

export const useNestedArray: (x:nestedArrayT) => nestedArrayT = GenTypeSatisfiesExpectedErrorsJS.useNestedArray as any;

export const useString: (x:stringT) => stringT = GenTypeSatisfiesExpectedErrorsJS.useString as any;

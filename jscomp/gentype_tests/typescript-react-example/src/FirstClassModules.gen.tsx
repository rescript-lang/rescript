/* TypeScript file generated from FirstClassModules.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore: Implicit any on import
import * as FirstClassModulesBS__Es6Import from './FirstClassModules.bs';
const FirstClassModulesBS: any = FirstClassModulesBS__Es6Import;

// eslint-disable-next-line consistent-type-definitions
export type MT_t = number;

// eslint-disable-next-line consistent-type-definitions
export type MT_InnerModule3_inner = number;

// eslint-disable-next-line consistent-type-definitions
export type firstClassModule = {
  readonly x: number; 
  readonly EmptyInnerModule: {
  }; 
  readonly InnerModule2: {
    readonly k: MT_t
  }; 
  readonly InnerModule3: {
    readonly k3: (_1:MT_InnerModule3_inner) => MT_InnerModule3_inner
  }; 
  readonly Z: unknown; 
  readonly y: string
};

export const firstClassModule: firstClassModule = FirstClassModulesBS.firstClassModule;

export const testConvert: (m:{
  readonly x: number; 
  readonly EmptyInnerModule: {
  }; 
  readonly InnerModule2: {
    readonly k: MT_t
  }; 
  readonly InnerModule3: {
    readonly k3: ((_1:MT_InnerModule3_inner) => MT_InnerModule3_inner)
  }; 
  readonly Z: unknown; 
  readonly y: string
}) => {
  readonly x: number; 
  readonly EmptyInnerModule: {
  }; 
  readonly InnerModule2: {
    readonly k: MT_t
  }; 
  readonly InnerModule3: {
    readonly k3: (_1:MT_InnerModule3_inner) => MT_InnerModule3_inner
  }; 
  readonly Z: unknown; 
  readonly y: string
} = FirstClassModulesBS.testConvert;

export const someFunctorAsFunction: (x:{
  readonly x: number; 
  readonly EmptyInnerModule: {
  }; 
  readonly InnerModule2: {
    readonly k: MT_t
  }; 
  readonly InnerModule3: {
    readonly k3: ((_1:MT_InnerModule3_inner) => MT_InnerModule3_inner)
  }; 
  readonly Z: unknown; 
  readonly y: string
}) => { readonly ww: string } = FirstClassModulesBS.someFunctorAsFunction;

/* TypeScript file generated from BucklescriptAnnotations.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line consistent-type-definitions
export type someMutableFields = {
  mutable0: string; 
  readonly immutable: number; 
  mutable1: string; 
  mutable2: string
};

// eslint-disable-next-line consistent-type-definitions
export type someMethods = {
  readonly send: (_1:string) => void; 
  readonly on: (_1:string, _2:((_1:number) => void)) => void; 
  readonly threeargs: (_1:number, _2:string, _3:number) => string; 
  readonly twoArgs: (_1:number, _2:string) => number
};

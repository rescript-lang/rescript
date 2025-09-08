/* TypeScript file generated from Docstrings.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as DocstringsJS from './Docstrings.res.js';

export type t = "A" | "B";

/** hello */
export const flat: number = DocstringsJS.flat as any;

/** * Sign a message with a key.
  *
  * @param message - A message to be signed
  * @param key - The key with which to sign the message
  * @returns A signed message */
export const signMessage: (message:string, key:number) => string = DocstringsJS.signMessage as any;

export const one: (a:number) => number = DocstringsJS.one as any;

export const two: (a:number, b:number) => number = DocstringsJS.two as any;

export const tree: (a:number, b:number, c:number) => number = DocstringsJS.tree as any;

export const oneU: (a:number) => number = DocstringsJS.oneU as any;

export const twoU: (a:number, b:number) => number = DocstringsJS.twoU as any;

export const treeU: (a:number, b:number, c:number) => number = DocstringsJS.treeU as any;

export const useParam: (param:number) => number = DocstringsJS.useParam as any;

export const useParamU: (param:number) => number = DocstringsJS.useParamU as any;

export const unnamed1: (param:number) => number = DocstringsJS.unnamed1 as any;

export const unnamed1U: (param:number) => number = DocstringsJS.unnamed1U as any;

export const unnamed2: (param_0:number, param_1:number) => number = DocstringsJS.unnamed2 as any;

export const unnamed2U: (param_0:number, param_1:number) => number = DocstringsJS.unnamed2U as any;

export const grouped: (x:number, y:number, a:number, b:number, c:number, z:number) => number = DocstringsJS.grouped as any;

export const unitArgWithoutConversion: () => string = DocstringsJS.unitArgWithoutConversion as any;

export const unitArgWithoutConversionU: () => string = DocstringsJS.unitArgWithoutConversionU as any;

export const unitArgWithConversion: () => t = DocstringsJS.unitArgWithConversion as any;

export const unitArgWithConversionU: () => t = DocstringsJS.unitArgWithConversionU as any;

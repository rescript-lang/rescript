/* TypeScript file generated from Uncurried.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as UncurriedJS from './Uncurried.res.js';

export type u0 = () => string;

export type u1 = (_1:number) => string;

export type u2 = (_1:number, _2:string) => string;

export type u3 = (_1:number, _2:string, _3:number) => string;

export type auth = { readonly login: () => string };

export type authU = { readonly loginU: () => string };

export const uncurried0: () => string = UncurriedJS.uncurried0 as any;

export const uncurried1: (x:number) => string = UncurriedJS.uncurried1 as any;

export const uncurried2: (x:number, y:string) => string = UncurriedJS.uncurried2 as any;

export const uncurried3: (x:number, y:string, z:number) => string = UncurriedJS.uncurried3 as any;

export const curried3: (x:number, y:string, z:number) => string = UncurriedJS.curried3 as any;

export const callback: (cb:(() => number)) => string = UncurriedJS.callback as any;

export const callback2: (auth:auth) => string = UncurriedJS.callback2 as any;

export const callback2U: (auth:authU) => string = UncurriedJS.callback2U as any;

export const sumU: (n:number, m:number) => void = UncurriedJS.sumU as any;

export const sumU2: (n:number) => (_1:number) => void = UncurriedJS.sumU2 as any;

export const sumCurried: (n:number) => (_1:number) => void = UncurriedJS.sumCurried as any;

export const sumLblCurried: (s:string, n:number) => (m:number) => void = UncurriedJS.sumLblCurried as any;

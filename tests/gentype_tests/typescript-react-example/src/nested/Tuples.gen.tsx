/* TypeScript file generated from Tuples.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as TuplesJS from './Tuples.res.js';

export type coord = [number, number, (undefined | number)];

export type coord2 = [number, number, (null | undefined | number)];

export type person = { readonly name: string; readonly age: number };

export type couple = [person, person];

export const testTuple: (param:[number, number]) => number = TuplesJS.testTuple as any;

export const origin: [number, number, (undefined | number)] = TuplesJS.origin as any;

export const computeArea: (param:[number, number, (undefined | number)]) => number = TuplesJS.computeArea as any;

export const computeAreaWithIdent: (param:coord) => number = TuplesJS.computeAreaWithIdent as any;

export const computeAreaNoConverters: (param:[number, number]) => number = TuplesJS.computeAreaNoConverters as any;

export const coord2d: <T1,T2,T3>(x:T1, y:T2) => [T1, T2, (undefined | T3)] = TuplesJS.coord2d as any;

export const getFirstName: (param:couple) => string = TuplesJS.getFirstName as any;

export const marry: (first:person, second:person) => couple = TuplesJS.marry as any;

export const changeSecondAge: (param:couple) => couple = TuplesJS.changeSecondAge as any;

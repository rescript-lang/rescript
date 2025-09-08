/* TypeScript file generated from References.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as ReferencesJS from './References.res.js';

export abstract class R_t<a> { protected opaque!: a }; /* simulate opaque types */

export type t<a> = R_t<a>;

export type requiresConversion = { readonly x: number };

export const create: (x:number) => { contents: number } = ReferencesJS.create as any;

export const access: (r:{ contents: number }) => number = ReferencesJS.access as any;

export const update: (r:{ contents: number }) => void = ReferencesJS.update as any;

export const get: <T1>(_1:R_t<T1>) => T1 = ReferencesJS.get as any;

export const make: <T1>(_1:T1) => R_t<T1> = ReferencesJS.make as any;

export const set: <T1>(_1:R_t<T1>, _2:T1) => void = ReferencesJS.set as any;

export const destroysRefIdentity: (x:{ contents: requiresConversion }) => { contents: requiresConversion } = ReferencesJS.destroysRefIdentity as any;

export const preserveRefIdentity: (x:R_t<requiresConversion>) => R_t<requiresConversion> = ReferencesJS.preserveRefIdentity as any;

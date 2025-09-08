/* TypeScript file generated from TestPromise.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as TestPromiseJS from './TestPromise.res.js';

export type promise<a> = Promise<a>;

export type fromPayload = { readonly x: number; readonly s: string };

export type toPayload = { readonly result: string };

export const convert: (p:Promise<fromPayload>) => Promise<toPayload> = TestPromiseJS.convert as any;

export const barx: (x:(undefined | Promise<(undefined | string)>), _2:void) => boolean = TestPromiseJS.barx as any;

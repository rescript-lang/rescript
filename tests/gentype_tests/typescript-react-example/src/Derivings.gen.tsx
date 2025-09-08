/* TypeScript file generated from Derivings.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as DerivingsJS from './Derivings.res.js';

export type action = 
    "Click"
  | "Cancel"
  | { TAG: "Submit"; _0: string };

export const click: action = DerivingsJS.click as any;

export const submit: (_1:string) => action = DerivingsJS.submit as any;

export const cancel: action = DerivingsJS.cancel as any;

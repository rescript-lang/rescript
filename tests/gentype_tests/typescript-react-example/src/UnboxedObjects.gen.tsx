/* TypeScript file generated from UnboxedObjects.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as UnboxedObjectsJS from './UnboxedObjects.res.js';

export type createdKind = "created";

export type stoppedKind = "stopped" | "cancelled";

export type created = { readonly kind: createdKind; readonly id: number };

export type stopped = { readonly kind: stoppedKind; readonly reason: string };

export type named = created | stopped;

export type inline = 
    { readonly kind: createdKind; readonly id: number }
  | { readonly kind: stoppedKind; readonly reason: string };

export type mixed = 
    created
  | { readonly kind: stoppedKind; readonly reason: string };

export type domains = 
    { readonly kind: string; readonly value: number }
  | { readonly kind: number; readonly amount: number }
  | { readonly kind: boolean; readonly enabled: boolean };

export type precedence = 
    { readonly kind: createdKind; readonly value: number }
  | { readonly kind: string; readonly value: number };

export const read: (value:named) => number = UnboxedObjectsJS.read as any;

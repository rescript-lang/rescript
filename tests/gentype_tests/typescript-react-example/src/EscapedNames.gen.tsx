/* TypeScript file generated from EscapedNames.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as EscapedNamesJS from './EscapedNames.res.js';

export type variant = "Illegal\"Name";

export type UppercaseVariant = "Illegal\"Name";

export type polymorphicVariant = "Illegal\"Name";

export type object_ = { readonly normalField: number; readonly "escape\"me": number };

export type record = {
  readonly normalField: variant; 
  readonly "Renamed'Field": number; 
  readonly "Illegal-field name": number; 
  readonly UPPERCASE: number
};

export const myRecord: record = EscapedNamesJS.myRecord as any;

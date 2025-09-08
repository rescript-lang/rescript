/* TypeScript file generated from NestedModules.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as NestedModulesJS from './NestedModules.res.js';

export type Universe_nestedType = string[];

export type Universe_Nested2_nested2Type = Array<string[]>;

export type Universe_Nested2_Nested3_nested3Type = Array<Array<string[]>>;

export type Universe_variant = "A" | { TAG: "B"; _0: string };

export const notNested: number = NestedModulesJS.notNested as any;

export const Universe_theAnswer: number = NestedModulesJS.Universe.theAnswer as any;

export const Universe_Nested2_nested2Value: number = NestedModulesJS.Universe.Nested2.nested2Value as any;

export const Universe_Nested2_Nested3_nested3Value: string = NestedModulesJS.Universe.Nested2.Nested3.nested3Value as any;

export const Universe_Nested2_Nested3_nested3Function: (x:Universe_Nested2_nested2Type) => Universe_Nested2_nested2Type = NestedModulesJS.Universe.Nested2.Nested3.nested3Function as any;

export const Universe_Nested2_nested2Function: (x:Universe_Nested2_Nested3_nested3Type) => Universe_Nested2_Nested3_nested3Type = NestedModulesJS.Universe.Nested2.nested2Function as any;

export const Universe_someString: string = NestedModulesJS.Universe.someString as any;

export const Universe: {
  theAnswer: number; 
  Nested2: {
    nested2Function: (x:Universe_Nested2_Nested3_nested3Type) => Universe_Nested2_Nested3_nested3Type; 
    nested2Value: number; 
    Nested3: {
      nested3Value: string; 
      nested3Function: (x:Universe_Nested2_nested2Type) => Universe_Nested2_nested2Type
    }
  }; 
  someString: string
} = NestedModulesJS.Universe as any;

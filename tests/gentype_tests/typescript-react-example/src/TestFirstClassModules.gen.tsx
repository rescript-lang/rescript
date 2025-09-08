/* TypeScript file generated from TestFirstClassModules.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as TestFirstClassModulesJS from './TestFirstClassModules.res.js';

import type {firstClassModule as FirstClassModulesInterface_firstClassModule} from './FirstClassModulesInterface.gen';

import type {firstClassModule as FirstClassModules_firstClassModule} from './FirstClassModules.gen';

import type {record as FirstClassModulesInterface_record} from './FirstClassModulesInterface.gen';

export type firstClassModuleWithTypeEquations<i,o> = { readonly out: (_1:o) => o; readonly Inner: { readonly inn: (_1:i) => i } };

export const convert: (x:FirstClassModules_firstClassModule) => FirstClassModules_firstClassModule = TestFirstClassModulesJS.convert as any;

export const convertInterface: (x:FirstClassModulesInterface_firstClassModule) => FirstClassModulesInterface_firstClassModule = TestFirstClassModulesJS.convertInterface as any;

export const convertRecord: (x:FirstClassModulesInterface_record) => FirstClassModulesInterface_record = TestFirstClassModulesJS.convertRecord as any;

export const convertFirstClassModuleWithTypeEquations: <T1,T2>(x:{ readonly out: ((_1:T1) => T1); readonly Inner: { readonly inn: ((_1:T2) => T2) } }) => { readonly out: (_1:T1) => T1; readonly Inner: { readonly inn: (_1:T2) => T2 } } = TestFirstClassModulesJS.convertFirstClassModuleWithTypeEquations as any;

/* TypeScript file generated from TestEmitInnerModules.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as TestEmitInnerModulesJS from './TestEmitInnerModules.res.js';

export const Inner_x: number = TestEmitInnerModulesJS.Inner.x as any;

export const Inner_y: string = TestEmitInnerModulesJS.Inner.y as any;

export const Outer_Medium_Inner_y: number = TestEmitInnerModulesJS.Outer.Medium.Inner.y as any;

export const Inner: { x: number; y: string } = TestEmitInnerModulesJS.Inner as any;

export const Outer: { Medium: { Inner: { y: number } } } = TestEmitInnerModulesJS.Outer as any;

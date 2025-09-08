/* TypeScript file generated from Opaque.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as OpaqueJS from './Opaque.res.js';

import type {business as Records_business} from './Records.gen';

export abstract class opaqueFromRecords { protected opaque!: any }; /* simulate opaque types */

export type pair = [opaqueFromRecords, opaqueFromRecords];

export const noConversion: (x:opaqueFromRecords) => opaqueFromRecords = OpaqueJS.noConversion as any;

export const testConvertNestedRecordFromOtherFile: (x:Records_business) => Records_business = OpaqueJS.testConvertNestedRecordFromOtherFile as any;

/* TypeScript file generated from StdlibNoShims.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as StdlibNoShimsJS from './StdlibNoShims.res.js';

export const idInt: (x:number) => number = StdlibNoShimsJS.idInt as any;

export const idFloat: (x:number) => number = StdlibNoShimsJS.idFloat as any;

export const idBool: (x:boolean) => boolean = StdlibNoShimsJS.idBool as any;

export const idString: (x:string) => string = StdlibNoShimsJS.idString as any;

export const idBigInt: (x:bigint) => bigint = StdlibNoShimsJS.idBigInt as any;

export const idDate: (x:Date) => Date = StdlibNoShimsJS.idDate as any;

export const idRegExp: (x:RegExp) => RegExp = StdlibNoShimsJS.idRegExp as any;

export const idPromise: (x:Promise<string>) => Promise<string> = StdlibNoShimsJS.idPromise as any;

export const idDict: (x:{[id: string]: number}) => {[id: string]: number} = StdlibNoShimsJS.idDict as any;

export const idMap: (x:Map<string,number>) => Map<string,number> = StdlibNoShimsJS.idMap as any;

export const idWeakMap: (x:WeakMap<number[],number>) => WeakMap<number[],number> = StdlibNoShimsJS.idWeakMap as any;

export const idSet: (x:Set<string>) => Set<string> = StdlibNoShimsJS.idSet as any;

export const idWeakSet: (x:WeakSet<number[]>) => WeakSet<number[]> = StdlibNoShimsJS.idWeakSet as any;

export const idArray: (x:number[]) => number[] = StdlibNoShimsJS.idArray as any;

export const idUndefined: (x:(undefined | number)) => (undefined | number) = StdlibNoShimsJS.idUndefined as any;

export const idNull: (x:(null | number)) => (null | number) = StdlibNoShimsJS.idNull as any;

export const idNullable: (x:(null | undefined | number)) => (null | undefined | number) = StdlibNoShimsJS.idNullable as any;

export const idOption: (x:(undefined | string)) => (undefined | string) = StdlibNoShimsJS.idOption as any;

export const idJSON: (x:unknown) => unknown = StdlibNoShimsJS.idJSON as any;

export const idResult: (x:
    { TAG: "Ok"; _0: number }
  | { TAG: "Error"; _0: string }) => 
    { TAG: "Ok"; _0: number }
  | { TAG: "Error"; _0: string } = StdlibNoShimsJS.idResult as any;

export const idResultAlias: (x:
    { TAG: "Ok"; _0: number }
  | { TAG: "Error"; _0: string }) => 
    { TAG: "Ok"; _0: number }
  | { TAG: "Error"; _0: string } = StdlibNoShimsJS.idResultAlias as any;

export const idRef: (x:{ contents: number }) => { contents: number } = StdlibNoShimsJS.idRef as any;

export const returnsUnit: () => void = StdlibNoShimsJS.returnsUnit as any;

export const idTuple: (x:[number, string, number]) => [number, string, number] = StdlibNoShimsJS.idTuple as any;

export const idArrayBuffer: (x:ArrayBuffer) => ArrayBuffer = StdlibNoShimsJS.idArrayBuffer as any;

export const idDataView: (x:DataView) => DataView = StdlibNoShimsJS.idDataView as any;

export const idInt8Array: (x:Int8Array) => Int8Array = StdlibNoShimsJS.idInt8Array as any;

export const idUint8Array: (x:Uint8Array) => Uint8Array = StdlibNoShimsJS.idUint8Array as any;

export const idUint8ClampedArray: (x:Uint8ClampedArray) => Uint8ClampedArray = StdlibNoShimsJS.idUint8ClampedArray as any;

export const idInt16Array: (x:Int16Array) => Int16Array = StdlibNoShimsJS.idInt16Array as any;

export const idUint16Array: (x:Uint16Array) => Uint16Array = StdlibNoShimsJS.idUint16Array as any;

export const idInt32Array: (x:Int32Array) => Int32Array = StdlibNoShimsJS.idInt32Array as any;

export const idUint32Array: (x:Uint32Array) => Uint32Array = StdlibNoShimsJS.idUint32Array as any;

export const idFloat32Array: (x:Float32Array) => Float32Array = StdlibNoShimsJS.idFloat32Array as any;

export const idFloat64Array: (x:Float64Array) => Float64Array = StdlibNoShimsJS.idFloat64Array as any;

export const idBigInt64Array: (x:BigInt64Array) => BigInt64Array = StdlibNoShimsJS.idBigInt64Array as any;

export const idBigUint64Array: (x:BigUint64Array) => BigUint64Array = StdlibNoShimsJS.idBigUint64Array as any;

export const idSymbol: (x:symbol) => symbol = StdlibNoShimsJS.idSymbol as any;

export const idIterator: (x:Iterator<number>) => Iterator<number> = StdlibNoShimsJS.idIterator as any;

export const idAsyncIterator: (x:AsyncIterator<number>) => AsyncIterator<number> = StdlibNoShimsJS.idAsyncIterator as any;

export const idOrdering: (x:number) => number = StdlibNoShimsJS.idOrdering as any;

export const idIntlCollator: (x:Intl.Collator) => Intl.Collator = StdlibNoShimsJS.idIntlCollator as any;

export const idIntlDateTimeFormat: (x:Intl.DateTimeFormat) => Intl.DateTimeFormat = StdlibNoShimsJS.idIntlDateTimeFormat as any;

export const idIntlListFormat: (x:Intl.ListFormat) => Intl.ListFormat = StdlibNoShimsJS.idIntlListFormat as any;

export const idIntlLocale: (x:Intl.Locale) => Intl.Locale = StdlibNoShimsJS.idIntlLocale as any;

export const idIntlNumberFormat: (x:Intl.NumberFormat) => Intl.NumberFormat = StdlibNoShimsJS.idIntlNumberFormat as any;

export const idIntlPluralRules: (x:Intl.PluralRules) => Intl.PluralRules = StdlibNoShimsJS.idIntlPluralRules as any;

export const idIntlRelativeTimeFormat: (x:Intl.RelativeTimeFormat) => Intl.RelativeTimeFormat = StdlibNoShimsJS.idIntlRelativeTimeFormat as any;

export const idIntlSegmenter: (x:Intl.Segmenter) => Intl.Segmenter = StdlibNoShimsJS.idIntlSegmenter as any;

export const idIntlSegments: (x:Intl.Segments) => Intl.Segments = StdlibNoShimsJS.idIntlSegments as any;

export const idJsError: (x:Error) => Error = StdlibNoShimsJS.idJsError as any;

export const idObj: (x:{}) => {} = StdlibNoShimsJS.idObj as any;

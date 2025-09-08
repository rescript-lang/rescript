// Import the generated bindings to force TS to resolve types without shims
import * as S from "./StdlibNoShims.gen";

// primitives
const i: number = S.idInt(1);
const f: number = S.idFloat(1.5);
const b: boolean = S.idBool(true);

// string
const s: string = S.idString("hello");

// bigint
const bi: bigint = S.idBigInt(1n);

// Date
const d: Date = S.idDate(new Date());

// RegExp
const re: RegExp = S.idRegExp(/a/);

// Promise
const p: Promise<string> = S.idPromise(Promise.resolve("ok"));

// Dict
const dict: { [id: string]: number } = S.idDict({ a: 1, b: 2 });

// Map / WeakMap
const m: Map<string, number> = S.idMap(new Map<string, number>());
const wm: WeakMap<number[], number> = S.idWeakMap(
  new WeakMap<number[], number>()
);

// Set / WeakSet
const set: Set<string> = S.idSet(new Set<string>());
const wset: WeakSet<number[]> = S.idWeakSet(new WeakSet<number[]>());

// undefined / null / nullable
const u: undefined | number = S.idUndefined(undefined);
const n: null | number = S.idNull(null);
const nu: null | undefined | number = S.idNullable(undefined);

// option
const opt: string | undefined = S.idOption(undefined);

// JSON
const j: unknown = S.idJSON({});

// Result
const ok: { TAG: "Ok"; _0: number } = { TAG: "Ok", _0: 1 };
const err: { TAG: "Error"; _0: string } = { TAG: "Error", _0: "e" };
const r1 = S.idResult(ok);
const r2 = S.idResult(err);

// result alias
const r3 = S.idResultAlias(ok);
const r4 = S.idResultAlias(err);

// array
const arr: number[] = S.idArray([1, 2, 3]);

// ref
const refv: { contents: number } = S.idRef({ contents: 1 });

// unit return
const voidReturn: void = S.returnsUnit();

// ArrayBuffer / DataView
const ab: ArrayBuffer = S.idArrayBuffer(new ArrayBuffer(8));
const dv: DataView = S.idDataView(new DataView(ab));

// Typed arrays
const i8: Int8Array = S.idInt8Array(new Int8Array());
const u8: Uint8Array = S.idUint8Array(new Uint8Array());
const u8c: Uint8ClampedArray = S.idUint8ClampedArray(new Uint8ClampedArray());
const i16: Int16Array = S.idInt16Array(new Int16Array());
const u16: Uint16Array = S.idUint16Array(new Uint16Array());
const i32: Int32Array = S.idInt32Array(new Int32Array());
const u32: Uint32Array = S.idUint32Array(new Uint32Array());
const f32: Float32Array = S.idFloat32Array(new Float32Array());
const f64: Float64Array = S.idFloat64Array(new Float64Array());
const bi64: BigInt64Array = S.idBigInt64Array(new BigInt64Array(2));
const bu64: BigUint64Array = S.idBigUint64Array(new BigUint64Array(2));

// Symbol (may not be mapped yet)
const sym: symbol = S.idSymbol(Symbol("x"));

// Iterator / AsyncIterator / Ordering
const it: Iterator<number> = S.idIterator([1, 2, 3].values());
const ait: AsyncIterator<number> = S.idAsyncIterator({
  next(): Promise<IteratorResult<number>> {
    return Promise.resolve({ done: true, value: undefined });
  },
});
const ord: number = S.idOrdering(0);

// Intl family
const _coll: Intl.Collator = S.idIntlCollator(new Intl.Collator());
const _dtf: Intl.DateTimeFormat = S.idIntlDateTimeFormat(
  new Intl.DateTimeFormat()
);
const _lf: Intl.ListFormat = S.idIntlListFormat(new Intl.ListFormat());
const _loc: Intl.Locale = S.idIntlLocale(new Intl.Locale("en-US"));
const _nf: Intl.NumberFormat = S.idIntlNumberFormat(new Intl.NumberFormat());
const _pr: Intl.PluralRules = S.idIntlPluralRules(new Intl.PluralRules());
const _rtf: Intl.RelativeTimeFormat = S.idIntlRelativeTimeFormat(
  new Intl.RelativeTimeFormat()
);
const _seg: Intl.Segmenter = S.idIntlSegmenter(new Intl.Segmenter());
const _segs: Intl.Segments = S.idIntlSegments(
  new Intl.Segmenter().segment("hello")
);

// Errors (use JsError only)
const _jserr: Error = S.idJsError(new Error("x"));

// Generic object
const _obj: {} = S.idObj({ a: 1, b: "x" });

// Tuple
const _tup: [number, string, number] = S.idTuple([1, "x", 1.5]);

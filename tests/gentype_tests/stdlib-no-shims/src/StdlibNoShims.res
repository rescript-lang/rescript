// Verify Stdlib.* types map without TS shims

@genType
let idInt = (x: int) => x

@genType
let idFloat = (x: float) => x

@genType
let idBool = (x: bool) => x

@genType
let idString = (x: String.t) => x

@genType
let idBigInt = (x: bigint) => x

@genType
let idDate = (x: Date.t) => x

@genType
let idRegExp = (x: RegExp.t) => x

@genType
let idPromise = (x: Promise.t<string>) => x

@genType
let idDict = (x: Dict.t<int>) => x

@genType
let idMap = (x: Map.t<string, int>) => x

@genType
let idWeakMap = (x: WeakMap.t<array<int>, int>) => x

@genType
let idSet = (x: Set.t<string>) => x

@genType
let idWeakSet = (x: WeakSet.t<array<int>>) => x

@genType
let idArray = (x: array<int>) => x

@genType
let idUndefined = (x: undefined<int>) => x

@genType
let idNull = (x: Null.t<int>) => x

@genType
let idNullable = (x: Nullable.t<int>) => x

@genType
let idOption = (x: option<string>) => x

@genType
let idJSON = (x: JSON.t) => x

@genType
let idResult = (x: Result.t<int, string>) => x

@genType
let idResultAlias = (x: result<int, string>) => x

@genType
let idRef = (x: ref<int>) => x

@genType
let returnsUnit = (): unit => ()
@genType
let idTuple = (x: (int, string, float)) => x

// Typed arrays and related JS interop types
@genType let idArrayBuffer = (x: ArrayBuffer.t) => x
@genType let idDataView = (x: DataView.t) => x

@genType let idInt8Array = (x: Int8Array.t) => x
@genType let idUint8Array = (x: Uint8Array.t) => x
@genType let idUint8ClampedArray = (x: Uint8ClampedArray.t) => x
@genType let idInt16Array = (x: Int16Array.t) => x
@genType let idUint16Array = (x: Uint16Array.t) => x
@genType let idInt32Array = (x: Int32Array.t) => x
@genType let idUint32Array = (x: Uint32Array.t) => x
@genType let idFloat32Array = (x: Float32Array.t) => x
@genType let idFloat64Array = (x: Float64Array.t) => x
@genType let idBigInt64Array = (x: BigInt64Array.t) => x
@genType let idBigUint64Array = (x: BigUint64Array.t) => x

// Additional stdlib types
@genType let idSymbol = (x: Symbol.t) => x

// More Stdlib exposed types (add more as generator support grows)
@genType let idIterator = (x: Iterator.t<int>) => x
@genType let idAsyncIterator = (x: AsyncIterator.t<int>) => x
@genType let idOrdering = (x: Ordering.t) => x

// Intl* types
@genType let idIntlCollator = (x: Intl.Collator.t) => x
@genType let idIntlDateTimeFormat = (x: Intl.DateTimeFormat.t) => x
@genType let idIntlListFormat = (x: Intl.ListFormat.t) => x
@genType let idIntlLocale = (x: Intl.Locale.t) => x
@genType let idIntlNumberFormat = (x: Intl.NumberFormat.t) => x
@genType let idIntlPluralRules = (x: Intl.PluralRules.t) => x
@genType let idIntlRelativeTimeFormat = (x: Intl.RelativeTimeFormat.t) => x
@genType let idIntlSegmenter = (x: Intl.Segmenter.t) => x
@genType let idIntlSegments = (x: Intl.Segments.t) => x

// Errors
@genType let idJsError = (x: JsError.t) => x

// Dynamic object
@genType let idObj = (x: {..}) => x

// dummy change to trigger rebuild

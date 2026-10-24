open Mocha
open Test_utils

/***
Pins the rules stated in compiler/ml/UNTAGGED_VARIANTS.md. Each block names
the rule it pins. Values are passed through [id], whose [@inline(never)] keeps
them away from constant folding, so the assertions observe the generated
dispatch. [raw] injects a JavaScript value a case's type does not describe;
those assertions show how recognition treats values outside every case.
*/

@inline(never) let id = x => x
external raw: 'a => 'b = "%identity"

// Rule 1: `@unboxed` and `@tag` on the type apply to every constructor;
// `@unboxed` on one constructor of a tagged variant applies to that one.
type mixed = | @unboxed MA(int) | MB({x: int}) | MC
let mixedName = v =>
  switch v {
  | MA(_) => "MA"
  | MB(_) => "MB"
  | MC => "MC"
  }

// Rule 2: a nullary case is its literal; a payload case is its payload.
@unboxed
type literals =
  | Named
  | @as("s") Str
  | @as(1) One
  | @as(2.5) Half
  | @as(3n) Three
  | @as(true) True
  | @as(null) Null
  | @as(undefined) Undefined
  | Payload({x: int})
@unboxed @tag("kind") type tagIgnored = TI({x: int}) | TS(string)
@unboxed type asIgnored = | @as("x") AsX(string) | AsY(int)

// Rule 4: payload kinds, one case of each kind recognized by `typeof`.
@unboxed
type kinds =
  | KString(string)
  | KNumber(float)
  | KBigint(bigint)
  | KBool(bool)
  | KFunction(int => int)
  | KObject({x: int})
let kindName = v =>
  switch v {
  | KString(_) => "string"
  | KNumber(_) => "number"
  | KBigint(_) => "bigint"
  | KBool(_) => "bool"
  | KFunction(_) => "function"
  | KObject(_) => "object"
  }

// Rule 4: arrays, tuples and dicts.
@unboxed type tuples = Tuple((int, string)) | TDict(dict<int>)
let tupleName = v =>
  switch v {
  | Tuple(_) => "array"
  | TDict(_) => "object"
  }
type myDict = Dict.t<int>
@unboxed type aliasDict = ADict(myDict) | AArray(array<int>)
let aliasDictName = v =>
  switch v {
  | ADict(_) => "object"
  | AArray(_) => "array"
  }

// Rules 4 and 5: instance cases, written directly and through an alias.
type myDate = Date.t
@unboxed
type instances =
  | IDate(myDate)
  | IRegExp(RegExp.t)
  | IPromise(promise<int>)
  | IMap(Map.t<int, int>)
  | ISet(Set.t<int>)
  | IWeakMap(WeakMap.t<{"a": int}, int>)
  | IWeakSet(WeakSet.t<{"a": int}>)
  | IArrayBuffer(ArrayBuffer.t)
  | IDataView(DataView.t)
  | IFloat64(Float64Array.t)
  | IString(string)
let instanceName = v =>
  switch v {
  | IDate(_) => "Date"
  | IRegExp(_) => "RegExp"
  | IPromise(_) => "Promise"
  | IMap(_) => "Map"
  | ISet(_) => "Set"
  | IWeakMap(_) => "WeakMap"
  | IWeakSet(_) => "WeakSet"
  | IArrayBuffer(_) => "ArrayBuffer"
  | IDataView(_) => "DataView"
  | IFloat64(_) => "Float64Array"
  | IString(_) => "string"
  }

// Rule 4: a payload of no recognized kind is the only payload case.
@unboxed type unknown = | @as(null) UNull | @as("none") UNone | UList(list<int>)
let unknownName = v =>
  switch v {
  | UNull => "null"
  | UNone => "none"
  | UList(_) => "list"
  }

// Rule 7: literal cases are tested first.
@unboxed type overlapString = | @as("a") LitA | AnyString(string)
let overlapStringName = v =>
  switch v {
  | LitA => "LitA"
  | AnyString(_) => "AnyString"
  }
@unboxed type overlapNumber = | @as(1) LitOne | AnyInt(int)
let overlapNumberName = v =>
  switch v {
  | LitOne => "LitOne"
  | AnyInt(_) => "AnyInt"
  }
@unboxed type overlapBool = | @as(true) LitTrue | AnyBool(bool)
let overlapBoolName = v =>
  switch v {
  | LitTrue => "LitTrue"
  | AnyBool(_) => "AnyBool"
  }
@unboxed type overlapNull = | @as(null) LitNull | AnyObject({x: int})
let overlapNullName = v =>
  switch v {
  | LitNull => "LitNull"
  | AnyObject(_) => "AnyObject"
  }

// Rule 8: instance checks precede the `typeof` checks.
@unboxed type objectFirst = OObject({x: int}) | ODate(Date.t)
let objectFirstName = v =>
  switch v {
  | OObject(_) => "object"
  | ODate(_) => "Date"
  }

// Rule 8: an int case is recognized as any number.
@unboxed type intOrString = I(int) | S(string)
let intOrStringName = v =>
  switch v {
  | I(_) => "int"
  | S(_) => "string"
  }

// Rule 9: `null` and arrays do not reach the object case.
@unboxed type guarded = GArray(array<int>) | GObject({x: int}) | @as(null) GNull
let guardedName = v =>
  switch v {
  | GArray(_) => "array"
  | GObject(_) => "object"
  | GNull => "null"
  }
let guardedObjectOnly = v =>
  switch v {
  | GObject(_) => "object"
  | _ => "other"
  }

// Rule 10: in a two-way test the second case has no check.
@unboxed type twoInstances = TDate(Date.t) | TRegExp(RegExp.t)
let twoInstancesName = v =>
  switch v {
  | TDate(_) => "Date"
  | TRegExp(_) => "RegExp"
  }
@unboxed type promiseOrFloat = PPromise(promise<int>) | PFloat(float)
let promiseOrFloatName = v =>
  switch v {
  | PPromise(_) => "promise"
  | PFloat(_) => "float"
  }

// Rule 12: `Some(v)` is `v` unless the variant can hold `undefined`.
@unboxed type withNull = | @as(null) WNull | WString(string)
@inline(never) let someWithNull = (v: withNull): option<withNull> => Some(v)
@unboxed type withUndefined = | @as(undefined) WUndefined | WUString(string)
@inline(never) let someWithUndefined = (v: withUndefined): option<withUndefined> => Some(v)
let someConstantUndefined: option<withUndefined> = Some(WUndefined)
type opaque
@unboxed type withOpaque = | @as("n") ONone | OPaque(opaque)
@inline(never) let someWithOpaque = (v: withOpaque): option<withOpaque> => Some(v)
@unboxed type withVariable<'a> = | @as("n") VNone | Var('a)
@inline(never) let someWithVariable = (v: withVariable<int>): option<withVariable<int>> => Some(v)
@unboxed type withBlob = BBlob(Blob.t) | BString(string)
@inline(never) let someWithBlob = (v: withBlob): option<withBlob> => Some(v)
@unboxed type withDate = DDate(Date.t) | DString(string)
@inline(never) let someWithDate = (v: withDate): option<withDate> => Some(v)
// A record payload holds `undefined` only through the field of an `@unboxed`
// record or of a one-field inline record.
type record = {y: option<int>}
@unboxed type withRecord = RRecord(record) | RString(string)
@inline(never) let someWithRecord = (v: withRecord): option<withRecord> => Some(v)
@unboxed type withInlineRecord = INone | IRecord({x: option<int>})
@inline(never)
let someWithInlineRecord = (v: withInlineRecord): option<withInlineRecord> => Some(v)
@unboxed type unboxedOption = {x: option<int>}
@unboxed type withUnboxedOption = | @as("n") UONone | UOption(unboxedOption)
@inline(never)
let someWithUnboxedOption = (v: withUnboxedOption): option<withUnboxedOption> => Some(v)
@unboxed type unboxedString = {s: string}
@unboxed type withUnboxedString = | @as("n") USNone | UString(unboxedString)
@inline(never)
let someWithUnboxedString = (v: withUnboxedString): option<withUnboxedString> => Some(v)

// Rule 13: `@notUndefined` on an abstract type.
@notUndefined type defined
@unboxed type withDefined = | @as("n") DNone | Defined(defined)
@inline(never) let someWithDefined = (v: withDefined): option<withDefined> => Some(v)

// Rule 14: `@tag` names the tag field of a tagged variant.
@tag("kind") type tagged = TA({x: int}) | @as("bee") TB({y: int}) | TC

describe(__MODULE__, () => {
  test("rule 1: @unboxed on one constructor of a tagged variant", () => {
    eq(__LOC__, raw(id(MA(3))), 3)
    eq(__LOC__, raw(id(MB({x: 1}))), {"TAG": "MB", "x": 1})
    eq(__LOC__, raw(id(MC)), "MC")
    eq(__LOC__, mixedName(id(MA(3))), "MA")
    eq(__LOC__, mixedName(id(MB({x: 1}))), "MB")
    eq(__LOC__, mixedName(id(MC)), "MC")
  })

  test("rule 2: representation of each case", () => {
    eq(__LOC__, raw(id(Named)), "Named")
    eq(__LOC__, raw(id(Str)), "s")
    eq(__LOC__, raw(id(One)), 1)
    eq(__LOC__, raw(id(Half)), 2.5)
    eq(__LOC__, raw(id(Three)), 3n)
    eq(__LOC__, raw(id(True)), true)
    eq(__LOC__, raw(id(Null)), Nullable.null)
    eq(__LOC__, raw(id(Undefined)), Nullable.undefined)
    eq(__LOC__, raw(id(Payload({x: 1}))), {"x": 1})
    eq(__LOC__, raw(id(TI({x: 1}))), {"x": 1})
    eq(__LOC__, raw(id(AsX("v"))), "v")
  })

  test("rule 4: typeof kinds", () => {
    eq(__LOC__, kindName(id(KString("a"))), "string")
    eq(__LOC__, kindName(id(KNumber(1.5))), "number")
    eq(__LOC__, kindName(id(KBigint(1n))), "bigint")
    eq(__LOC__, kindName(id(KBool(false))), "bool")
    eq(__LOC__, kindName(id(KFunction(x => x))), "function")
    eq(__LOC__, kindName(id(KObject({x: 1}))), "object")
  })

  test("rule 4: tuples are array cases, dicts are object cases", () => {
    eq(__LOC__, tupleName(id(Tuple((1, "a")))), "array")
    eq(__LOC__, tupleName(id(TDict(dict{"a": 1}))), "object")
    eq(__LOC__, aliasDictName(id(ADict(dict{"a": 1}))), "object")
    eq(__LOC__, aliasDictName(id(AArray([1]))), "array")
  })

  test("rules 4 and 5: instance cases", () => {
    eq(__LOC__, instanceName(id(IDate(Date.make()))), "Date")
    eq(__LOC__, instanceName(id(IRegExp(/a/))), "RegExp")
    eq(__LOC__, instanceName(id(IPromise(Promise.resolve(1)))), "Promise")
    eq(__LOC__, instanceName(id(IMap(Map.make()))), "Map")
    eq(__LOC__, instanceName(id(ISet(Set.make()))), "Set")
    eq(__LOC__, instanceName(id(IWeakMap(WeakMap.make()))), "WeakMap")
    eq(__LOC__, instanceName(id(IWeakSet(WeakSet.make()))), "WeakSet")
    let buffer = ArrayBuffer.make(8)
    eq(__LOC__, instanceName(id(IArrayBuffer(buffer))), "ArrayBuffer")
    eq(__LOC__, instanceName(id(IDataView(DataView.fromBuffer(buffer)))), "DataView")
    eq(__LOC__, instanceName(id(IFloat64(Float64Array.fromArray([1.0])))), "Float64Array")
    eq(__LOC__, instanceName(id(IString("a"))), "string")
  })

  test("rule 4: the unknown case receives every non-literal value", () => {
    eq(__LOC__, unknownName(id(UNull)), "null")
    eq(__LOC__, unknownName(id(UNone)), "none")
    eq(__LOC__, unknownName(id(UList(list{1}))), "list")
    eq(__LOC__, unknownName(id(UList(list{}))), "list")
  })

  test("rule 7: a payload value equal to a literal is that literal's case", () => {
    eq(__LOC__, overlapStringName(id(AnyString("a"))), "LitA")
    eq(__LOC__, overlapStringName(id(AnyString("b"))), "AnyString")
    eq(__LOC__, overlapNumberName(id(AnyInt(1))), "LitOne")
    eq(__LOC__, overlapNumberName(id(AnyInt(2))), "AnyInt")
    eq(__LOC__, overlapBoolName(id(AnyBool(true))), "LitTrue")
    eq(__LOC__, overlapBoolName(id(AnyBool(false))), "AnyBool")
    eq(__LOC__, overlapNullName(id(LitNull)), "LitNull")
    eq(__LOC__, overlapNullName(id(AnyObject({x: 1}))), "AnyObject")
  })

  test("rule 8: instance checks precede typeof checks", () => {
    eq(__LOC__, objectFirstName(id(ODate(Date.make()))), "Date")
    eq(__LOC__, objectFirstName(id(OObject({x: 1}))), "object")
  })

  test("rule 8: an int case receives any number", () => {
    eq(__LOC__, intOrStringName(id(I(1))), "int")
    eq(__LOC__, intOrStringName(raw(id(1.5))), "int")
    eq(__LOC__, intOrStringName(id(S("1"))), "string")
  })

  test("rule 9: null and arrays are not objects", () => {
    eq(__LOC__, guardedName(id(GArray([]))), "array")
    eq(__LOC__, guardedName(id(GNull)), "null")
    eq(__LOC__, guardedName(id(GObject({x: 1}))), "object")
    eq(__LOC__, guardedObjectOnly(id(GArray([]))), "other")
    eq(__LOC__, guardedObjectOnly(id(GNull)), "other")
    eq(__LOC__, guardedObjectOnly(id(GObject({x: 1}))), "object")
  })

  test("rule 10: a value outside every case lands in the unchecked branch", () => {
    eq(__LOC__, intOrStringName(raw(true)), "string")
    eq(__LOC__, twoInstancesName(raw({"x": 1})), "RegExp")
    let thenable = %raw(`{then: (resolve) => resolve(1)}`)
    eq(__LOC__, promiseOrFloatName(raw(thenable)), "float")
  })

  test("rule 12: option of an untagged variant", () => {
    eq(__LOC__, raw(someWithNull(WNull)), Nullable.null)
    eq(__LOC__, someWithNull(WNull) == None, false)
    eq(__LOC__, someWithUndefined(id(WUndefined)) == None, false)
    eq(__LOC__, someConstantUndefined == None, false)
    eq(__LOC__, raw(someWithUndefined(WUString("a"))), "a")
    // An undefined payload shows whether `Some` boxes: a boxed `Some` stays
    // distinct from `None`, an unboxed one does not.
    eq(__LOC__, someWithOpaque(OPaque(raw(Nullable.undefined))) == None, false)
    eq(__LOC__, someWithVariable(Var(raw(Nullable.undefined))) == None, false)
    eq(__LOC__, someWithBlob(BBlob(raw(Nullable.undefined))) == None, false)
    eq(__LOC__, someWithDate(DDate(raw(Nullable.undefined))) == None, true)
    eq(__LOC__, someWithRecord(RRecord(raw(Nullable.undefined))) == None, true)
    eq(__LOC__, someWithInlineRecord(raw(Nullable.undefined)) == None, false)
    eq(__LOC__, someWithUnboxedOption(UOption(raw(Nullable.undefined))) == None, false)
    eq(__LOC__, someWithUnboxedString(UString(raw(Nullable.undefined))) == None, true)
  })

  test("rule 13: @notUndefined unboxes option of a variant holding the type", () => {
    eq(__LOC__, someWithDefined(Defined(raw(Nullable.undefined))) == None, true)
    eq(__LOC__, raw(someWithDefined(Defined(raw(5)))), 5)
  })

  test("rule 14: @tag and @as on a tagged variant", () => {
    eq(__LOC__, raw(id(TA({x: 1}))), {"kind": "TA", "x": 1})
    eq(__LOC__, raw(id(TB({y: 1}))), {"kind": "bee", "y": 1})
    eq(__LOC__, raw(id(TC)), "TC")
  })
})

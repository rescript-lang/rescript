@genType
type t = int

@genType
let someIntList = list{1, 2, 3}

@genType
let map = List.map

@genType
type typeWithVars<'x, 'y, 'z> =
  | A('x, 'y)
  | B('z)

@genType
type rec tree = {"label": string, "left": option<tree>, "right": option<tree>}

/*
 * A tree is a recursive type which does not require any conversion (JS object).
 * All is well.
 */
@genType
let rec swap = (tree: tree): tree =>
  {
    "label": tree["label"],
    "left": tree["right"]->Belt.Option.map(swap),
    "right": tree["left"]->Belt.Option.map(swap),
  }

@genType
type rec selfRecursive = {self: selfRecursive}

@genType
type rec mutuallyRecursiveA = {b: mutuallyRecursiveB}
and mutuallyRecursiveB = {a: mutuallyRecursiveA}

/*
 * This is a recursive type which requires conversion (a record).
 * Only a shallow conversion of the top-level element is performed.
 */
@genType
let selfRecursiveConverter = ({self}) => self

/*
 * This is a mutually recursive type which requires conversion (a record).
 * Only a shallow conversion of the two top-level elements is performed.
 */
@genType
let mutuallyRecursiveConverter = ({b}) => b

@genType
let testFunctionOnOptionsAsArgument = (a: option<'a>, foo) => foo(a)

@genType.opaque
type opaqueVariant =
  | A
  | B

@genType
let stringT: String.t = "a"

@genType
let jsStringT: Js.String.t = "a"

@genType
let jsString2T: Js.String2.t = "a"

@genType
type twice<'a> = ('a, 'a)

@gentype
type genTypeMispelled = int

@genType
type dictString = Js.Dict.t<string>

@genType
let jsonStringify = Js.Json.stringify

@genType
type nullOrString = Js.Null.t<string>

@genType
type nullOrString2 = Js.null<string>

type record = {
  i: int,
  s: string,
}

@genType
let testConvertNull = (x: Js.Null.t<record>) => x

@genType
type decorator<'a, 'b> = 'a => 'b constraint 'a = int constraint 'b = _ => _

/* Bucklescript's marshaling rules. */
@genType
type marshalFields = {
  "_rec": string,
  "_switch": string,
  "switch": string,
  "__": string,
  "___": string,
  "foo__": string,
  "_foo__": string,
  "_Uppercase": string,
  "_Uppercase__": string,
}

@genType
let testMarshalFields: marshalFields = {
  "_rec": "rec",
  "_switch" /* reason keywords are not recognized */: "_switch",
  "switch": "switch",
  "__": "__",
  "___": "_",
  "foo__": "foo",
  "_foo__": "_foo",
  "_Uppercase": "Uppercase",
  "_Uppercase__": "_Uppercase",
}

@genType
type marshalMutableField = {@set "_match": int}

@genType
let setMatch = (x: marshalMutableField) => x["_match"] = 34

type ocaml_array<'a> = array<'a>

// This should be considered annotated automatically.
type someRecord = {id: int}

type instantiateTypeParameter = ocaml_array<someRecord>

@genType
let testInstantiateTypeParameter = (x: instantiateTypeParameter) => x

@genType @genType.as("Vector")
type vector<'a> = ('a, 'a)

@genType
type date = Js.Date.t

@genType
let currentTime = Js.Date.make()

@genType
type i64A = Int64.t

@genType
type i64B = int64

@genType
let i64Const: i64B = 34L

@genType
let optFunction = Some(() => 3)

module ObjectId: {
  @genType
  type t = int
} = {
  type t = int
  let x = 1
}

@@warning("-32")

@genType type t = int

@genType let someIntList = list{1, 2, 3}

@genType let map = Belt.List.map

@genType
type typeWithVars<'x, 'y, 'z> =
  | A('x, 'y)
  | B('z)

@genType type rec tree = {"label": string, "left": option<tree>, "right": option<tree>}

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

@genType type rec selfRecursive = {self: selfRecursive}

@genType type rec mutuallyRecursiveA = {b: mutuallyRecursiveB}
and mutuallyRecursiveB = {a: mutuallyRecursiveA}

/*
 * This is a recursive type which requires conversion (a record).
 * Only a shallow conversion of the top-level element is performed.
 */
@genType let selfRecursiveConverter = ({self}) => self

/*
 * This is a mutually recursive type which requires conversion (a record).
 * Only a shallow conversion of the two top-level elements is performed.
 */
@genType let mutuallyRecursiveConverter = ({b}) => b

@genType let testFunctionOnOptionsAsArgument = (a: option<'a>, foo) => foo(a)

@genType.opaque
type opaqueVariant =
  | A
  | B

@genType let stringT: string = "a"

@genType let jsStringT: Js.String.t = "a"

@genType let jsString2T: Js.String2.t = "a"

@genType type twice<'a> = ('a, 'a)

@gentype
type genTypeMispelled = int

@genType type dictString = dict<string>

@genType let jsonStringify = Js.Json.stringify

@genType type nullOrString = null<string>

@genType type nullOrString2 = Null.t<string>

@genType type nullOrString3 = Js.null<string>

@genType type nullOrString4 = Js.Null.t<string>

@genType type nullableOrString = nullable<string>

@genType type nullableOrString2 = Nullable.t<string>

@genType type nullableOrString3 = Js.nullable<string>

@genType type nullableOrString4 = Js.Nullable.t<string>

@genType type undefinedOrString = undefined<string>

@genType type undefinedOrString2 = Undefined.t<string>

@genType type undefinedOrString3 = Js.undefined<string>

@genType type undefinedOrString4 = Js.Undefined.t<string>

type record = {
  i: int,
  s: string,
}

@genType let testConvertNull = (x: Js.Null.t<record>) => x

@genType type decorator<'a, 'b> = 'a => 'b constraint 'a = int constraint 'b = _ => _

@genType let testConvertLocation = (x: Location.t) => x

/* ReScript's marshaling rules. */
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

@genType type marshalMutableField = {@set "_match": int}

@genType let setMatch = (x: marshalMutableField) => x["_match"] = 34

type ocaml_array<'a> = array<'a>

// This should be considered annotated automatically.
type someRecord = {id: int}

type instantiateTypeParameter = ocaml_array<someRecord>

@genType let testInstantiateTypeParameter = (x: instantiateTypeParameter) => x

@genType @genType.as("Vector")
type vector<'a> = ('a, 'a)

@genType type date = Js.Date.t

@genType let currentTime = Js.Date.make()

@genType let optFunction = Some(() => 3)

module ObjectId: {
  @genType type t = int
} = {
  type t = int
  let x = 1
}

@genType type tPrimed = (TypeNameSanitize.t', TypeNameSanitize.M.t'')

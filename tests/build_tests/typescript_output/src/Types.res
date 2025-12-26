// Record with @as renaming
@as("User")
type user = {
  @as("user_name") name: string,
  @as("user_age") age: int,
}

// Simple variant - nullary uses string literals
type status = Active | Inactive | @as("pending_review") PendingReview

// Variant with payload - uses TAG and _0
type result2 = Ok2(int) | Error2(string)

// Variant with inline record - flattens fields
type event =
  | Click({x: int, y: int})
  | KeyPress({key: string, shift: bool})
  | NoData

// @tag customizes the tag field name
@tag("type")
type action =
  | Navigate(string)
  | Submit

// @unboxed variant - no TAG, just the payload type
@unboxed
type stringOrInt = String(string) | Int(int)

let defaultUser: user = {name: "Anonymous", age: 0}
let myStatus: status = Active
let myResult: result2 = Ok2(42)
let myEvent: event = Click({x: 10, y: 20})
let myAction: action = Navigate("/home")
let myStringOrInt: stringOrInt = String("hello")

// Type aliases
type intArray = array<int>
type stringList = list<string>
type optionalInt = option<int>
type intResult = result<int, string>
type stringDict = dict<string>
type intRef = ref<int>

// Nested aliases
type nestedOption = option<option<int>>
type arrayOfOptions = array<option<string>>
type optionOfArray = option<array<int>>

// Alias of alias
type myIntArray = intArray

// Values using aliases
let myIntArray: intArray = [1, 2, 3]
let myStringList: stringList = list{"a", "b", "c"}
let myOptionalInt: optionalInt = Some(42)
let myIntResult: intResult = Ok(100)
let myStringDict: stringDict = dict{"key": "value"}
let myIntRef: intRef = ref(10)
let myNestedOption: nestedOption = Some(Some(5))
let myArrayOfOptions: arrayOfOptions = [Some("hello"), None]
let myOptionOfArray: optionOfArray = Some([1, 2, 3])
let myAliasOfAlias: myIntArray = [4, 5, 6]

// Opaque types - pure abstract types (phantom types)
type valid
type invalid

// Opaque type with phantom type parameter
type validated<'s> = string

// Example value using opaque types
let validatedValue: validated<valid> = "test"

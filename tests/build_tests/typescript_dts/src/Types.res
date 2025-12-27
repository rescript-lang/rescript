// Record with @as renaming
@as("User")
type user = {
  @as("user_name") name: string,
  @as("user_age") age: int,
}

// Simple variant - nullary uses string literals
type status = Active | Inactive | @as("pending_review") PendingReview

// Variant with payload
type result2 = Ok2(int) | Error2(string)

// Variant with inline record
type event =
  | Click({x: int, y: int})
  | KeyPress({key: string, shift: bool})
  | NoData

// @tag customizes the tag field name
@tag("type")
type action =
  | Navigate(string)
  | Submit

// @unboxed variant
@unboxed
type stringOrInt = String(string) | Int(int)

let defaultUser: user = {name: "Anonymous", age: 0}

// Opaque types - pure abstract types
type valid
type invalid

// Opaque type with phantom type parameter
type validated<'s> = string

// Function using opaque types
let validate: validated<invalid> => validated<valid> = s => s
let getValue: validated<'s> => string = s => s

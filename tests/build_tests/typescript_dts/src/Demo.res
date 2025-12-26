// Basic functions
let add = (a, b) => a + b
let greet = name => "Hello, " ++ name
let identity = x => x

// Option handling
let processOption = (opt: option<int>): int =>
  switch opt {
  | Some(v) => v
  | None => 0
  }

// Unit return (no params in TS)
let getUnit = () => ()
let getValue = () => 42

// Record type
type record = {foo: string, bar: int}
let myRecord: record = {foo: "hello", bar: 42}

// Polymorphic variants - nullary
type color = [#red | #green | #blue]
let myColor: color = #red

// Polymorphic variants - with payload
type colorWithPayload = [#hex(string) | #rgb(int, int, int)]
let myRgb: colorWithPayload = #rgb(255, 0, 0)

// Object types
let jsObj: {"name": string, "age": int} = {"name": "Alice", "age": 30}

// Labeled parameters
let withLabels = (~name: string, ~age: int) => name ++ " is " ++ Int.toString(age)

// Optional parameters
let withOptional = (~name: string, ~prefix: option<string>=?) =>
  switch prefix {
  | Some(p) => p ++ name
  | None => name
  }

// Constants
let myNumber = 42
let myString = "hello"

// Record with function fields
type eventHandler = {
  onClick: unit => unit,
  getValue: unit => int,
  transform: int => string,
}
let handler: eventHandler = {
  onClick: () => (),
  getValue: () => 42,
  transform: n => Int.toString(n),
}

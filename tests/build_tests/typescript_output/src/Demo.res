// Basic functions
let add = (a, b) => a + b
let greet = name => "Hello, " ++ name
let identity = x => x
let asyncIdentity = async x => x

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
type colorWithPayload = [#hex(string) | #rgb(int, int, int) | #named(string)]
let myRgb: colorWithPayload = #rgb(255, 0, 0)

// Object types (mutable, no readonly)
let jsObj: {"name": string, "age": int} = {"name": "Alice", "age": 30}

// Labeled parameters
let withLabels = (~name: string, ~age: int) => name ++ " is " ++ Int.toString(age)

// Optional parameters (unwrapped from option)
let withOptional = (~name: string, ~prefix: option<string>=?) =>
  switch prefix {
  | Some(p) => p ++ name
  | None => name
  }

// Dict type -> Record<string, T>
let myDict: Dict.t<int> = Dict.make()

// Constants
let myNumber = 42
let myString = "hello"

// Result type
let myOk: result<int, string> = Ok(42)
let myError: result<int, string> = Error("oops")

// Ref type
let myRef: ref<int> = ref(0)

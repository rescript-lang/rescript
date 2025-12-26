// Simple module with type and value
module Inner = {
  type t = int
  let value: t = 42
}

// Module with multiple values
module Math = {
  let add = (a, b) => a + b
  let multiply = (a, b) => a * b
}

// Nested modules
module Outer = {
  module Nested = {
    type t = string
    let greet = (name: t) => "Hello, " ++ name
  }
}

// Opaque type module pattern using @opaque attribute
// This creates a branded intersection type: string & { [$Opaque.Email$t]: [] }
module Email = {
  @opaque type t = string
  let make = (rawEmail: string): option<t> =>
    switch rawEmail->String.includes("@") {
    | true => Some(rawEmail)
    | false => None
    }
  let toString = (email: t): string => email
}

// Another opaque type module with same type name 't' to test no collision
module UserId = {
  @opaque type t = int
  let make = (id: int): t => id
  let toInt = (id: t): int => id
}

// Module with include - includes another module's contents
module ExtendedMath = {
  include Math
  let subtract = (a, b) => a - b
  let divide = (a, b) => a / b
}

// Module type (signature) definition
module type Printable = {
  type t
  let toString: t => string
}

// Module that implements a module type
module PrintableInt: Printable with type t = int = {
  type t = int
  let toString = x => Int.toString(x)
}

// Nested opaque type module using @opaque
module Outer2 = {
  module Token = {
    @opaque type t = string
    let create = (s: string): t => "token:" ++ s
    let value = (t: t): string => t
  }
}

// Use module values
let useInner = Inner.value
let sum = Math.add(1, 2)
let greeting = Outer.Nested.greet("World")
let validEmail = Email.make("test@example.com")

// Use extended module
let diff = ExtendedMath.subtract(10, 5)
let product = ExtendedMath.multiply(3, 4)

// Use printable module
let intString = PrintableInt.toString(42)

// Use nested opaque module
let token = Outer2.Token.create("secret")
// Note: tokenValue assignment would be optimized to just 'token' since value is identity,
// which causes type mismatch. Using ignore to avoid the issue.
let _ = Outer2.Token.value(token)

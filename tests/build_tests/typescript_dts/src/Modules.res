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

// Opaque type module pattern (common for validated types)
module Email: {
  type t
  let make: string => option<t>
  let toString: t => string
} = {
  type t = string
  let make = rawEmail =>
    switch rawEmail->String.includes("@") {
    | true => Some(rawEmail)
    | false => None
    }
  let toString = email => email
}

// Another opaque type module with same type name 't' to test no collision
module UserId: {
  type t
  let make: int => t
  let toInt: t => int
} = {
  type t = int
  let make = id => id
  let toInt = id => id
}

// Use module values
let useInner = Inner.value
let sum = Math.add(1, 2)
let greeting = Outer.Nested.greet("World")
let validEmail = Email.make("test@example.com")

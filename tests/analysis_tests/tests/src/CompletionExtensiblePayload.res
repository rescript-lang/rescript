// Payloads of extensions are typed where the extension is declared
type color = Red | Blue
type record = {c: color}
type CompletionExtensiblePayloadBase.attr +=
  | Color(color)
  | Boxed({b: color})
  | Wrapped(record)
  | Pair(color, CompletionExtensiblePayloadBase.shape)
  | Sized(CompletionExtensiblePayloadThird.size)

let v: CompletionExtensiblePayloadBase.attr = Color(Red)
type attr = CompletionExtensiblePayloadBase.attr

// let x: attr = Color()
//                     ^com

// switch v { | Color() }
//                    ^com

// let x: attr = Boxed({b: })
//                         ^com

// switch v { | Wrapped({c: }) }
//                          ^com

// let x: attr = Pair(Red, )
//                         ^com

// switch v { | Pair(Red, ) }
//                        ^com

// let x: attr = Sized()
//                     ^com

// switch v { | Sized() }
//                    ^com

// let x: attr = CompletionExtensiblePayloadBase.Shaped()
//                                                      ^com

// Same-named constructors are told apart by their module path
module A = {
  type CompletionExtensiblePayloadBase.attr += Value(bool)
}
module B = {
  type CompletionExtensiblePayloadBase.attr += Value(option<color>)
  // let x: attr = Value()
  //                     ^com
}

// Payload types local to a nested module
module M = {
  type local = X | Y
  type CompletionExtensiblePayloadBase.attr += InM(local)
}

// let x: attr = M.InM()
//                     ^com

// switch v { | M.InM() }
//                    ^com

// let x: attr = A.Value()
//                       ^com

// let x: attr = B.Value()
//                       ^com

// let x: attr = B.Value(Some())
//                            ^com

// switch v { | A.Value() }
//                      ^com

// switch v { | B.Value() }
//                      ^com

// let x: attr = CompletionExtensiblePayloadBase.Value()
//                                                     ^com

// let x: attr = CompletionExtensiblePayload.B.Value()
//                                                   ^com

// switch v { | CompletionExtensiblePayloadBase.Value() }
//                                                    ^com

// Unqualified, the constructor of the most recent open is meant
open A

// let x: attr = Value()
//                     ^com

// switch v { | Value() }
//                    ^com

open B

// let x: attr = Value()
//                     ^com

// switch v { | Value() }
//                    ^com

// A local declaration after an open shadows the opened constructor
type CompletionExtensiblePayloadBase.attr += Value(CompletionExtensiblePayloadThird.size)

// let x: attr = Value()
//                     ^com

// switch v { | Value() }
//                    ^com

// An extension in a module type declares no constructor
module type S = {
  type CompletionExtensiblePayloadBase.attr += Phantom
}

// let x: attr = P
//                ^com

// switch v
//         ^com

// An open after a local declaration shadows it in turn
open A

// let x: attr = Value()
//                     ^com

// switch v { | Value() }
//                    ^com

// A module alias names the constructor of the module it aliases
module AliasB = B

// let x: attr = AliasB.Value()
//                            ^com

// switch v { | AliasB.Value() }
//                           ^com

open AliasB

// let x: attr = Value()
//                     ^com

// switch v { | Value() }
//                    ^com

// An alias of another file's module
module Base = CompletionExtensiblePayloadBase

// let x: attr = Base.Shaped()
//                           ^com

// An open after a local module shadows it for qualified constructors too
module Outer = {
  module Inner = {
    type CompletionExtensiblePayloadBase.attr += Deep(bool)
  }
}
module Inner = {
  type CompletionExtensiblePayloadBase.attr += Deep(option<color>)
}
open Outer

// let x: attr = Inner.Deep()
//                          ^com

// switch v { | Inner.Deep() }
//                         ^com

let crossRef = References.x
//               ^ref

let crossRef2 = References.x

module Ref = References

let crossRef3 = References.x

let crossRefWithInterface = ReferencesWithInterface.x
//                             ^ref

let crossRefWithInterface2 = ReferencesWithInterface.x

module RefWithInterface = ReferencesWithInterface

let crossRefWithInterface3 = ReferencesWithInterface.x

let _ = RenameWithInterface.x
//           ^ren RenameWithInterfacePrime

let _ = RenameWithInterface.x
//                          ^ren xPrime

let typeDef = {TypeDefinition.item: "foobar"}
//   ^typ

let _ = DefinitionWithInterface.y
//                              ^def

type defT = DefinitionWithInterface.t
//                                  ^def

type defT2 = DefinitionWithInterface.t
//                                   ^typ

// DefinitionWithInterface.a
//                          ^com

let yy = DefinitionWithInterface.Inner.y
//                                     ^def
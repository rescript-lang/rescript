@module("meh") @taggedTemplate
external meh: (array<string>, array<string>) => string = "default"

// let x = meh`foo`.
//                  ^com

// let y = meh`bar`.len
//                     ^com

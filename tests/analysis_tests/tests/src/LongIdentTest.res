module Map = TableclothMap

let zz = Map.add
//           ^hov
// Triggers the processing of `Of(M)._t` and Lident.Apply ends up in the AST
// even though it's not expressible in ReScript syntax.
// This simulates ReScript projects with OCaml dependencies containing ident apply.

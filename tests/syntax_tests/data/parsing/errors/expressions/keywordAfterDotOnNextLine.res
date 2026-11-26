// A reserved word is a field name only on the line of the `.`, so the next
// binding survives while a field access is being written.
let f = user => {
  let name = user.
  let age = 2
  age
}

// The same holds for a label after `~`
let b = f(~
let c = 1

// `open M` on the next line is the declaration, not a field or an operand
let d = user.
open Belt
let e = x->
open! Belt
let g = {a:
exception Foo

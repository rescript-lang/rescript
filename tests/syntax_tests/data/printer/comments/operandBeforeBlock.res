// Line comments in an operand followed by a block stay in the operand
let x = switch [
  a, // c
  b,
] { | _ => 1 }

let x = switch list{
  a, // c
  ...b,
  ...c,
} { | _ => 1 }

let x = switch f(
  a, // c
  b,
) { | _ => 1 }

let x = switch a // c
{ | _ => 1 }

let x = switch a /* c */ { | _ => 1 }

let x = try [
  ...a, // c
  b,
] catch { | _ => 1 }

let x = try a // c
catch { | _ => 1 }

let x = if a // c
{ 1 } else { 2 }

let x = if c { 1 } else if a // c
{ 2 } else { 3 }

let x = while a // c
{ () }

let x = for i in a // c
to b // d
{ () }

let x = for y of [
  a, // c
  b,
] { () }

let x = async () => {
  for await y of a // c
  { () }
}

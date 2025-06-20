let "stringPattern" = ()
let "stringPattern" as s = ()
let ("stringPattern" : string) = ()
let ("stringPattern" : string) as s = ()

switch x {
| "stringPattern" => ()
| "stringPattern" as s => ()
| ("stringPattern" : string) as s => ()
| "\xff" => ()
| "\u2212" => ()
| "\0" => ()
| "\o123" => ()
}

for "stringPattern" in 0 to 10 { () }
for "stringPattern" as s in 0 to 10 { () }

for (("stringPattern") in 0 to 10) { () }
for (("stringPattern") as s in 0 to 10) { () }
for (("stringPattern" as s) in 0 to 10) { () }

let f = ("stringPattern") => ()
let f = ("stringPattern" as s) => ()
let f = (("stringPattern" as s)) => ()
let f = (("stringPattern" : string)) => ()
let f = (("stringPattern" : string) as s) => ()
let f = ("stringPattern" : string) => ()

let 1 = ()
let 1 as x = ()
let (1: int) = ()
let (1: int) as x = ()

switch x {
| 1 => ()
| 1 as x => ()
| (1 : int) => ()
| (1 : int) as x => ()
}

let f = (1) => ()
let f = (1 as x) => ()
let f = ((1: int)) => ()
let f = ((1: int) as x) => ()
let f = (1: int) => ()

for i in 0 to 10 { () }
for i as x in 0 to 10 { () }
for ((i) in 0 to 10) { () }
for ((i) as x in 0 to 10) { () }
for ((i as x) in 0 to 10) { () }

switch listPatterns {
| list{(true, pattern), ...patterns} =>
  let patterns = patterns->List.map(filterSpread)->List.rev
  makeListPattern(loc, patterns, Some(pattern))
| patterns =>
  let patterns = patterns->List.map(filterSpread)->List.rev
  makeListPattern(loc, patterns, None)
}

let _0 = 0x9A

let print = (ppf, i) =>
  switch i.stamp {
  | 0 => fprintf(ppf, "%s!", i.name)
  | -1 => fprintf(ppf, "%s#", i.name)
  | +1 => fprintf(ppf, "%s#", i.name)
  | -1. => fprintf(ppf, "%s#", i.name)
  | +1. => fprintf(ppf, "%s#", i.name)
  }

let -1 .. -1. = x

switch science {
| (1.12, -3.13) => true
| [1.12, -3.13] => true
| list{1.12, -3.13} => true
| {x: 1.12, y: -3.13} => true
| Constructor(1.12, -2.45) => true
| #Constuctor(1.12, -2.45) => true
| -4.15 as x => true
| -4.15 | +4.15 => true
| (-3.14 : float) => true
| exception 19.34 => true
| _ => false
}

switch literal {
| `literal` => true
| (`literal1`, `literal2`) => true
| [`literal1`, `literal2`] => true
| list{`literal1`, `literal2`} => true
| {x: `literal1`, y: `literal2`} => true
| Constructor(`literal1`, `literal2`) => true
| #Constuctor(`literal1`, `literal2`) => true
| `literal` as x => true
| `literal` | `literal` => true
| (`literal` : string) => true
| exception `literal` => true
| _ => false
}

let `literal constant` = x

for `literal constant` in 0 to 10 { () }

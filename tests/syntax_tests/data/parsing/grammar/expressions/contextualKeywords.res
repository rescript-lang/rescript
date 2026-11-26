// as, constraint, exception, external, mutable, of, open, private, rec and
// when are names except where the grammar uses them as keywords.

// Variables
let as = 1
let constraint = 2
let exception = 3
let external = 4
let mutable = 5
let of = 6
let open = 7
let private = 8
let rec = 9
let when = 10
let rec: int = 11
let rec rec = () => rec()

// Calls, pipes and field access: what follows can't follow the keyword
let x = open(file)
let x = open->close
let x = external.contents
let x = exception->Console.error
let x = when + of * as

// Shorthands
let next = {...dialog, open}
let options = {as, private, open: true}
let render = (~as, ~open=false, ~exception=?) => <Dialog as open ?exception />
let x = f(~as, ~open?, ~private=1)

// Callbacks
let f = errors => errors->Array.forEach(exception => Console.error(exception))

// for loops: the iterator and the iterable
for of of items {
  ()
}
for (exception of errors) {
  ()
}

// Blocks
let f = () => {
  let open = dialog.open
  open
}
let x = {open: true, private: 1}

// Statements: `open`, `exception` and `external` alone
let f = () => {
  ignore()
  (open)
  Belt.x
  (exception)
  Foo
}
(external)
r.x = 1

// The keywords are unchanged
let f = () => {
  open Belt
  exception Foo
  1
}
let x = {open M; 1}
for x of xs {
  ()
}

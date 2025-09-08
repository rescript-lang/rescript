type foo

@send
external on: (
  foo,
  @string
  [
    | #bar(unit => unit)
    | #foo(string => unit)
  ],
) => unit = "on"

let on1 = (foo, event) => on(foo, event)

/* FIXME */
let on2 = (foo, h, event) => on(foo, h(event))

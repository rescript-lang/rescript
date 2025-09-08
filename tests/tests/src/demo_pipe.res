type readline
@send
external on: (
  readline,
  @string
  [
    | #line(string => unit)
    | #close(unit => unit)
  ],
) => readline = "on"
let register = rl => rl->on(#line(x => Js.log(x)))->on(#close(() => Js.log("finished")))

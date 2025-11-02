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
let register = rl => rl->on(#line(x => Console.log(x)))->on(#close(() => Console.log("finished")))

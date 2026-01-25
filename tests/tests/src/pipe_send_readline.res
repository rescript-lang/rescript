/* should give a warning on unused attribute..   [@@bs.xx] */

type readline
@send
external on: (
  readline as 'self,
  @string
  [
    | #line(string => unit)
    | #close(unit => unit)
  ],
) => 'self = "on"
let u = rl => rl->on(#line(x => Console.log(x)))->on(#close(() => Console.log("finished")))

@send external send: ({"hi": int} as 'self, string) => 'self = "send"

let xx = (h): int => h->send("x")->(x => x["hi"])

let yy = h => h->send("x")

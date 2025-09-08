()->Js.Dict.empty->Js.Dict.set("hi", "hello")

type u = {mutable x: int}

{x: 3}.x = {
  Js.log("hi")
  2
}

let f = x => {x: x}.x = x + 1

let f = x =>
  {
    x: {
      Js.log("hi")
      x
    },
  }.x = x + 1

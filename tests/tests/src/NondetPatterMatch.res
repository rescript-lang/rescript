type action =
  | WithoutPayload1
  | WithoutPayload2
  | WithPayload2({y: int})
  | WithPayload3({y: int})
  | WithPayload5({y: int})
  | WithPayload6({x: int})
  | WithPayload7({y: int})
  | WithPayload8({x: int})

let f1 = (action: action) => {
  switch action {
  | WithPayload5(_)
  | WithPayload6(_)
  | WithPayload7(_)
  | WithPayload8(_) =>
    Console.log("hello")
  | _ => ()
  }
  42
}

let f2 = (action: action) => {
  switch action {
  | WithPayload5(_)
  | WithPayload6(_)
  | WithPayload7(_)
  | WithPayload8(_) =>
    Console.log("hello")
  | _ => ()
  }
  42
}

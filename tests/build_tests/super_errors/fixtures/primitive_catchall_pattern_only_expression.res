@tag("kind")
type t =
  | @as("one") One({thing: string})
  | @catch(string) Other({kind: string, thing: int})

let x = Other({kind: "one", thing: 12})

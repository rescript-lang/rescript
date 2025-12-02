@tag("kind")
type response =
  | @as(202) Ok202({code: int})
  | @as(200) Ok200({code: int})
  | @as(int) Other({@as("kind") kind: int, body: string})

let mk = Other({kind: 404, body: "x"})


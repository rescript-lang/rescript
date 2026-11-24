let scale = (x: int, ~by=2) => x * by

let greet = (~name, ~greeting=?) =>
  switch greeting {
  | Some(g) => g ++ " " ++ name
  | None => "Hello " ++ name
  }

let offset = (~from=0, x: int) => x - from

let clamp = (~lo=0, ~hi=100, x: int) =>
  if x < lo {
    lo
  } else if x > hi {
    hi
  } else {
    x
  }

let wrap = (x, ~tag=?) =>
  switch tag {
  | Some(t) => (t, x)
  | None => ("none", x)
  }

let useAll = () =>
  scale(3) + scale(3, ~by=4) + offset(5) + clamp(500) + String.length(greet(~name="a"))

type rec t<_> = I: t<int> | S: t<string>

let f = x =>
  switch x {
  | I => 1
  | S => "s"
  }

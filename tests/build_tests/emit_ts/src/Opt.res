let orZero = (o: option<int>) =>
  switch o {
  | Some(n) => n
  | None => 0
  }

let double = (o: option<int>) => Option.map(o, n => n * 2)

let firstPositive = (a: int, b: int) =>
  if a > 0 {
    Some(a)
  } else if b > 0 {
    Some(b)
  } else {
    None
  }

let none = (): option<int> => None

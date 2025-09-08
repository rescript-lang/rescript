let f = value =>
  switch Js.Nullable.isNullable(value) {
  | false => Some((Obj.magic(value): string))
  | true => None
  }

let fxx = v =>
  switch v() {
  | 1 => 'a'
  | 2 => 'b'
  | 3 => 'c'
  | _ => 'd'
  }

let fxxx2 = v =>
  switch v() {
  | false => 1
  | true => 2
  }

let fxxx3 = v =>
  if v() {
    2
  } else {
    1
  }

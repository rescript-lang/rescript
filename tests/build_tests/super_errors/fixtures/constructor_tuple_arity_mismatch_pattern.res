type binary = Binary(int, int)

let read = value =>
  switch value {
  | Binary((x, y)) => x + y
  }

type binary = Binary(int, int)

let read = value =>
  switch value {
  | Binary((x, y, z)) => x + y + z
  }

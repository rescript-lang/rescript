type record = {value: int}

@unboxed
type t = Empty | Number(int) | Text(string) | Object(record)

let value = Object({value: 42})
let read = value =>
  switch value {
  | Empty => 0
  | Number(number) => number
  | Text(text) => String.length(text)
  | Object(record) => record.value
  }

let answer = read(value)

type state =
  | Empty
  | Int1(int)
  | Int2(int)
let func = (state: state): int =>
  switch state {
  | Empty => 0
  | Int2(intValue)
  | Int1(intValue) =>
    let intFunc = intParam => intParam + intValue
    intFunc(0)
  }

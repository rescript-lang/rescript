let doStuff = (a: int, b: [#ONE | #TWO]) => {
  switch b {
  | #ONE => a + 1
  | #TWO => a + 2
  }
}

let x = doStuff(1, "ONE")

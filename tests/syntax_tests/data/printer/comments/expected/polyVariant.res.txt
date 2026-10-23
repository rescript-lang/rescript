let value = #Pair(a, /* between */ b)

let read = value =>
  switch value {
  | #Pair(a, /* between pattern */ b) => (a, b)
  }

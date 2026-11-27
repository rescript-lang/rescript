type source = {@as("0") s0: int, @as("1") s1: int, @as("2") s2: int}
type head = {@as("0") s0: int, @as("1") s1: int}

let split = (r: source) => {
  let {s2, ...head as rest} = r
  (s2, rest)
}

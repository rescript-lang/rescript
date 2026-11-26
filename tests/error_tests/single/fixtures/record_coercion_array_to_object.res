type pair = {@as("0") a: int, @as("1") b: int}
type tail = {@as("1") b: int}
@unboxed type v = Pair(pair) | Tail(tail)

let classify = (v: v) =>
  switch v {
  | Pair(_) => "pair"
  | Tail(_) => "tail"
  }

let fromPair = (r: pair) => classify(Tail(r :> tail))

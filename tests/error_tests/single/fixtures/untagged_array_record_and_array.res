type pair = {@as("0") x?: int, @as("1") y?: int}

@unboxed type v = Pair(pair) | List(array<int>)

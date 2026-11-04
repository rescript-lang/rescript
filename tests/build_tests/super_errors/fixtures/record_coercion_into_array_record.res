type obj = {@as("0") a: int, @as("1") b: int, x: int}
type arr = {@as("0") a: int, @as("1") b: int}

let toArr = (x: obj) => (x :> arr)

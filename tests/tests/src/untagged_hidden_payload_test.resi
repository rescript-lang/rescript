type t
@unboxed type v = | @as("n") N | P(t)
let make: unit => v
let isPayload: v => bool

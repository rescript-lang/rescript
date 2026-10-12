@unboxed type first = | @as(1) One | @as(2) Two
@unboxed type second = | @as(2) Again | @as(3) Three
@unboxed type t = First({kind: first, value: int}) | Second({kind: second, reason: string})

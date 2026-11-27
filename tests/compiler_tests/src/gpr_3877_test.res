@inline
let test = code =>
  switch code {
  | 201 | 200 => "good response"
  | 500
  | 503
  | 506
  | 509
  | 598
  | 501
  | 504
  | 507
  | 510
  | 599
  | 502
  | 505
  | 508
  | 511 => "bad response"
  | _ => "the catch all"
  }

let a = test(201)

let b = test(504)

assert(a == "good response")
assert(b == "bad response")

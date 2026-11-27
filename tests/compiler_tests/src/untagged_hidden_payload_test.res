// The interface hides the payload type, so its case P has an unknown payload,
// which agrees with the implementation's class payload
type t = Date.t
@unboxed type v = | @as("n") N | P(t)
let make = () => P(Date.make())

let isPayload = v =>
  switch v {
  | N => false
  | P(_) => true
  }

Mocha.describe(__MODULE__, () => {
  Mocha.test("an interface hiding a payload's class", () => {
    Test_utils.eq(__LOC__, true, isPayload(make()))
    Test_utils.eq(__LOC__, false, isPayload(N))
  })
})

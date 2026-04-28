let ok = (loc, a) => Node_assert.ok(a, ~message=loc)
let eq = (loc, a, b) => Node_assert.deepEqual(a, b, ~message=loc)
let throws = (loc, f) => Node_assert.throws(f, ~message=loc)

/**
Approximate equality comparison with a threshold parameter.
Returns true if the absolute difference between two values is less than or equal to the threshold.
*/
let approxEq = (loc, threshold, a, b) => {
  let diff = Js.Math.abs_float(a -. b)
  Node_assert.ok(diff <= threshold, ~message=loc)
}

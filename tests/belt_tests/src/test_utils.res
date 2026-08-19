let ok = (loc, a) => Node_assert.ok(a, ~message=loc)
let eq = (loc, a, b) => Node_assert.deepEqual(a, b, ~message=loc)
let throws = (loc, f) => Node_assert.throws(f, ~message=loc)

let approxEq = (loc, threshold, a, b) => {
  let diff = Math.abs(a -. b)
  Node_assert.ok(diff <= threshold, ~message=loc)
}

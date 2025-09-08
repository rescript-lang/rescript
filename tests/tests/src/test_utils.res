let ok = (loc, a) => Node_assert.ok(a, ~message=loc)
let eq = (loc, a, b) => Node_assert.deepEqual(a, b, ~message=loc)
let throw = (loc, f) => Node_assert.throws(f, ~message=loc)

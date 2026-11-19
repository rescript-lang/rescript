@throws(ThrowsErrors.NotFound)
let find = x => x > 0 ? x : throw(ThrowsErrors.NotFound)

let ppwarningOnce = @ppwarning("hello") 1

@warning("-102")
let suppressedBinding =
  [1] == [1]

let suppressedExpression = @warning("-102") ([2] == [2])

let comparesToNull = x => x == Nullable.null

let warns = [3] == [3]

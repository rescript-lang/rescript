// ^int

/** Doc of f. */
@deprecated("use g")
let f = x => x + 1
let g = x => x + 1

@genType @genType.as("renamed") let exported = 1

@live let kept = 2

/** Finds a positive number. */
@throws(Not_found)
let find = x => x > 0 ? x : throw(Not_found)

/** Recursive.
  Second line. */
let rec even = n => n == 0 || odd(n - 1)
/** Odd. */
@deprecated
and odd = n => n != 0 && even(n - 1)

let (first, second) = (1, "two")

/** Destructured. */
@deprecated("pair")
let (left, right) = (1, 2)

/** Record. */
let {contents} = ref(3)

let onExpression = @deprecated("ignored") (x => x)

@inline let inlined = 4

/** Inlined doc. */
@deprecated("use inlined") @inline
let inlinedDeprecated = 5

/** Annotated. */
let annotated: int => int = x => x

@warning("-27") let unused = x => 5

/** Several in one let. */
let a = 1
/** Second binding. */
and b = 2

module Nested = {
  /** Nested doc. */
  @deprecated("nested")
  let n = 1

  module Deeper = {
    /** Deeper doc.
```rescript
let d = "d"
```
    */
    let d = "d"
  }
}

module Make = (X: {}) => {
  /** In a functor. */
  let v = 1
}

module Applied = Make({})

module Comp = {
  /** A component. */
  @react.component
  let make = (~name: string) => React.string(name)
}

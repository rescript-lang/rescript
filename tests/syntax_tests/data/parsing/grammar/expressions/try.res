try {
  let x = 1
  let y = 2
  dangerousCall(x + y)
} catch {
| Foo => Console.log("caught Foo")
| Exit => Console.log("caught exit")
}

@attr
try myDangerousFn() catch {
| Foo => Console.log("caught Foo")
}

let x = {
  let y = 1
  try {
    apply(y)
  } catch {
  | _ => 2
  }
}

// Try-catch-finally with block expressions
try {
  riskyOperation()
} catch {
| Not_found => Console.log("Item not found")
| exn => Console.log("Other error")
} finally {
  Console.log("Cleanup complete")
}

// Try-catch-finally with comments
try {
  // This is a comment in the try block
  let result = fetchData()
  processResult(result)
} catch {
| ParseError => defaultValue
| ValidationError => fallbackValue
} finally {
  // Comment in finally block
  logProcessingComplete()
  releaseParser()
}

// Nested try-catch-finally
try {
  try {
    innerOperation()
  } catch {
  | InnerError => Console.log("Inner error handled")
  } finally {
    Console.log("Inner cleanup")
  }
} catch {
| OuterError => Console.log("Outer error handled")
} finally {
  Console.log("Outer cleanup")
}

// Try-finally without catch tests
let tryFinally1 = try doSomething() finally cleanup()

let tryFinally2 = try {
  let x = 1
  let y = 2
  dangerousOperation(x + y)
} finally {
  Console.log("Cleanup complete")
}

// Try-finally in various contexts
let tryFinallyInAssignment = {
  value: try computeValue() finally logComputation(),
  status: "computed"
}

let tryFinallyInArray = [
  try getItem(0) finally logAccess(0),
  try getItem(1) finally logAccess(1)
]

let tryFinallyInFunction = () => {
  try performOperation() finally logOperationComplete()
}

// Complex nested try-finally
let complex = {
  data: try {
    let raw = try parseJson(input) finally logParseAttempt()
    let validated = try validate(raw) finally logValidation()
    try transform(validated) finally logTransform()
  } catch {
  | ParseError => {error: "parse", value: null}
  | ValidationError => {error: "validation", value: null}
  } finally {
    Console.log("Overall cleanup")
  }
}

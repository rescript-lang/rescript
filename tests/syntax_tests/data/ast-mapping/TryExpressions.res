// Test for try..catch (existing syntax)
let tryCatch = try {
  Console.log("Trying")
} catch {
| _ => Console.log("Caught")
}

// Test for try..finally (new syntax)
let tryFinally = try {
  Console.log("Trying")
} finally {
  Console.log("Finally")
}

// Test for try..catch..finally (new syntax)
let tryCatchFinally = try {
  Console.log("Trying")
} catch {
| _ => Console.log("Caught")
} finally {
  Console.log("Finally")
}

// Test with complex expressions
let complexTry = try {
  let x = 1 + 2
  let y = x * 3
  Some(y)
} catch {
| Not_found => None
| exn => Console.log("Error: " ++ Js.String.make(exn))
}

// Test nested try expressions
let nestedTry = try {
  try {
    dangerousOperation()
  } catch {
  | InnerError => Console.log("Inner caught")
  }
} catch {
| OuterError => Console.log("Outer caught")
} finally {
  Console.log("Outer finally")
}

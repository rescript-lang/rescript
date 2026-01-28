// Basic finally block test

exception TestError

let testFinallyBasic = () => {
  try {
    1 + 1
  } catch {
  | _ => 0
  } finally {
    Console.log("Finally executed!")
  }
}

let testFinallyWithException = () => {
  try {
    throw(TestError)
  } catch {
  | TestError => 1
  } finally {
    Console.log("Finally with exception executed!")
  }
}

let testFinallyWithoutCatch = () => {
  try {
    42
  } catch {
  | _ => 0
  } finally {
    Console.log("Finally without catch executed!")
  }
}

let testFinallyWithAssignment = () => {
  let x = try {
    42
  } catch {
  | _ => 0
  } finally {
    Console.log("Finally with assignment executed!")
  }

  Console.log(x)
}

// Test single-line finally block
let testSingleLineFinally = () => {
  try {
    1 + 1
  } catch {
  | _ => 0
  } finally Console.log("Single line!")
}

// Test multi-line finally block
let testMultiLineFinally = () => {
  try {
    throw(TestError)
  } catch {
  | TestError => 1
  } finally {
    Console.log("Line 1")
    Console.log("Line 2")
    Console.log("Line 3")
  }
}

// Test try..finally without catch
let testTryFinallyWithoutCatch = () => {
  try {
    Console.log("Doing operation")
    42
  } finally {
    Console.log("Finally without catch executed!")
  }
}

let testTryFinallyWithoutCatchWithException = () => {
  try {
    Console.log("About to throw")
    throw(TestError)
  } finally {
    Console.log("Finally without catch with exception executed!")
  }
}

// Test try..finally without catch with assignment
let testTryFinallyWithoutCatchAssignment = () => {
  let x = try {
    Console.log("Getting value")
    42
  } finally {
    Console.log("Finally without catch with assignment executed!")
  }

  Console.log(x)
}

exception B

let rec loop = () => loop()

let throwInFinally = () =>
  try {
    1
  } finally {
    throw(B)
  }

let throwAfterStatementInFinally = () =>
  try {
    1
  } finally {
    Console.log(1)
    throw(B)
  }

let throwInFinallyAfterCatch = () =>
  try {
    1
  } catch {
  | B => 2
  } finally {
    throw(B)
  }

let assertFalseInFinally = () =>
  try {
    1
  } finally {
    assert(false)
  }

let nonreturningCallInFinally = () =>
  try {
    1
  } finally {
    loop()
  }

let unreachableCodeInFinally = () =>
  try {
    1
  } finally {
    throw(B)
    Console.log(2)
  }

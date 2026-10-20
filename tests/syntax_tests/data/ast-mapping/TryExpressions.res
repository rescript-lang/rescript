let tryCatch = () =>
  try {
    dangerous()
  } catch {
  | Not_found => 0
  }

let tryFinally = () =>
  try {
    dangerous()
  } finally {
    cleanup()
  }

let tryCatchFinally = () =>
  try {
    dangerous()
  } catch {
  | Not_found => 0
  } finally {
    cleanup()
  }

// The finally block keeps its attributes and the try keeps its own
let attributed = () =>
  @attr
  try {
    dangerous()
  } finally {
    @inner cleanup()
  }

let nested = () =>
  try {
    try {
      dangerous()
    } finally {
      inner()
    }
  } finally {
    outer()
  }

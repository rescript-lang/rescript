module Error2 = {
  type t = int
  let notification = n => (Int.toString(n), "")
}

module MyErrorHandler = ErrorHandler.Make(Error2) /* MyErrorHandler.notify(42) */

module Error2 = {
  type t = int
  let notification = n => (string_of_int(n), "")
}

module MyErrorHandler = ErrorHandler.Make(Error2) /* MyErrorHandler.notify(42) */


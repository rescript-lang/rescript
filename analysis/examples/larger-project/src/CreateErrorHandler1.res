module Error1 = {
  type t = string
  let notification = s => (s, s)
}

module MyErrorHandler = ErrorHandler.Make(Error1)

MyErrorHandler.notify("abc")

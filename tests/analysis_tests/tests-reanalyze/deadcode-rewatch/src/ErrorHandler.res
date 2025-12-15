module type Error = {
  type t
  let notification: t => (string, string)
}

module Make = (Error: Error) => {
  let notify = x => Error.notification(x)
}

// This is ignored as there's an interface file
@genType
let x = 42


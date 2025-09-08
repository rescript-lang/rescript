module type Error = {
  type t
  let notification: t => (string, string)
}
module Make: (Error: Error) =>
{
  let notify: Error.t => (string, string)
}

let x: int


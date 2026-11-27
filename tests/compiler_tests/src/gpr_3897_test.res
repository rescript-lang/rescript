module type S = {
  let x: int
}

module Make = (U: S) => {
  include U
}

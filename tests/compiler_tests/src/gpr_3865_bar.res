module type S = {
  let return: int
}

module Make = (M: S) => {
  include M
}

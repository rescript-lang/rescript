module Make = (
  X: {
    type t
  },
) => {
  @notUndefined type t = X.t
}

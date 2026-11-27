module Make = (
  X: {
    type t
  },
) => {
  @instanceOf("Date") type date = X.t
}

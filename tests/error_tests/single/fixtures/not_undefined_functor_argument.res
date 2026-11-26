module Make = (
  X: {
    @notUndefined type t
  },
) => {
  type u = X.t
}

module Applied = Make({
  type t = option<int>
})

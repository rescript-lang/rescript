module F = (
  M: {
    let v: int
  },
) => {
  let doubled = M.v * 2
}

open F

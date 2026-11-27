module F = (
  M: {
    let x: int
  },
) => {
  let v = M.x
}

let _: F.v = 1

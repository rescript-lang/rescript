let compute = () => 1

let f = () =>
  try {
    1
  } finally {
    compute()
  }

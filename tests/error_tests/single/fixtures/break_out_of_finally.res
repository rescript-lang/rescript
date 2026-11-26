let f = () =>
  while true {
    try {
      ()
    } finally {
      break
    }
  }

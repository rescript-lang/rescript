module Inner: {
  let make: unit => string
}

module Deep: {
  module Deep: {
    let make: unit => string
  }
}

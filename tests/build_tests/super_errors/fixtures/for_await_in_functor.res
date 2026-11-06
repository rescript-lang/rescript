module type Source = {
  let values: unit => AsyncGenerator.t<int, unit, unit>
}

module Make = (Source: Source) => {
  let values = []

  for await value of Source.values() {
    values->Array.push(value)
  }
}

module type Source = {
  let value: promise<int>
}

module type Result = {
  let value: int
}

module type Make = (Source: Source) => Result

let make = async (): module(Make) =>
  module(
    (Source: Source) => {
      let value = await Source.value
    }
  )

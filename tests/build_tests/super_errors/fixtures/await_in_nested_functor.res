module type Source = {
  let value: promise<int>
}

module Make = (Source: Source) => {
  module Inner = (Other: Source) => {
    let value = await Other.value
  }
}

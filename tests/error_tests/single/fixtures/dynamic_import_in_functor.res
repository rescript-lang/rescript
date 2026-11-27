module type Source = {
  let values: list<int>
}

module Make = (Source: Source) => {
  module L = await List
  let length = L.length(Source.values)
}

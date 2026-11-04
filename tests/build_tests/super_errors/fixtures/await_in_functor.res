module type Source = {
  let value: promise<int>
}

module Make = (Source: Source) => {
  let value = await Source.value
}

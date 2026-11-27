module type Source = {
  let value: promise<int>
}

module Make = (First: Source, Second: Source) => {
  let value = await Second.value
}

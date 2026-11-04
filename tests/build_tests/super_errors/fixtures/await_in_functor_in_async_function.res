module type Source = {
  let value: promise<int>
}

let read = async value => {
  module Make = (Source: Source) => {
    let value = await Source.value
  }
  module M = Make({
    let value = value
  })
  M.value
}

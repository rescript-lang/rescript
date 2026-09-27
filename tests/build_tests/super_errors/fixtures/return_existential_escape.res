type rec box = Box('a): box
let get = value => {
  switch value {
  | Box(x) => %return(x)
  }
}

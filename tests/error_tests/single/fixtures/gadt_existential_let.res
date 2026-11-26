type rec any = Any('a, 'a => string): any

let get = value => {
  let Any(x, _) = value
  x
}

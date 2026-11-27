module L = List

let v = x => {
  module H = List
  module U = L
  (H.length(x), U.length(x))
}

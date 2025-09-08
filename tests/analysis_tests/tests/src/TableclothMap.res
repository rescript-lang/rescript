let add = 3

module Of = (M: {}) => {
  type _t = int
}

module M = {}

module O = Of(M)
module Int = {
  type _t = O._t
}

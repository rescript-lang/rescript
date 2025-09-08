module Make = (
  S: {
    type elt
  },
) => {
  open S
  type t<'a>
  @get_index external unsafe_get: (t<elt>, int) => elt = ""

  @get_index external get: (t<elt>, int) => Js.undefined<elt> = ""

  let opt_get = (f, i) => Js.Undefined.toOption(get(f, i))
}

module Int_arr = Make({
  type elt = int
})

let f = (v): (int, option<_>) => (Int_arr.unsafe_get(v, 0), Int_arr.opt_get(v, 1))

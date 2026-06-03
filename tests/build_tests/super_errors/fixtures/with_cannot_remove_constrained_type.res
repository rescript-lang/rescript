module type S = {
  type t<'a> constraint 'a = int
}

module type T = S with type t<'a> := 'a

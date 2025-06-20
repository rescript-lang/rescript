let length = Primitive_array_extern.length

let get = (xs, index) =>
  if index < 0 || index >= length(xs) {
    throw(Invalid_argument("index out of bounds"))
  } else {
    xs->Primitive_array_extern.getUnsafe(index)
  }

let set = (xs, index, newval) =>
  if index < 0 || index >= length(xs) {
    throw(Invalid_argument("index out of bounds"))
  } else {
    xs->Primitive_array_extern.setUnsafe(index, newval)
  }

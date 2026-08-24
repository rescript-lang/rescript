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

// Note: this is exposed to support syntax
@new external makeUninitializedUnsafe: int => array<'a> = "Array"

let spread = arrays => {
  let arraysLength = length(arrays)
  let resultLength = ref(0)
  for i in 0 to arraysLength - 1 {
    resultLength.contents =
      resultLength.contents + length(Primitive_array_extern.getUnsafe(arrays, i))
  }

  let result = makeUninitializedUnsafe(resultLength.contents)
  resultLength.contents = 0
  for i in 0 to arraysLength - 1 {
    let array = Primitive_array_extern.getUnsafe(arrays, i)
    for j in 0 to length(array) - 1 {
      Primitive_array_extern.setUnsafe(
        result,
        resultLength.contents,
        Primitive_array_extern.getUnsafe(array, j),
      )
      resultLength.contents = resultLength.contents + 1
    }
  }
  result
}

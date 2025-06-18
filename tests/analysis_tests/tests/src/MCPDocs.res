type x = {test: bool}

module SomeModule = {
  let ff = {test: true}

  @unboxed
  type someVariant = One(int) | Two(string)
}

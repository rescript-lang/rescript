let f = (type a, x: a) => {
  module M = {
    @typeOf("object") type o = a
  }
  x
}

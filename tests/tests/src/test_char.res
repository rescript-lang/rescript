let caml_is_printable = c => {
  let code = Stdlib.Char.code(c)
  code > 31 && code < 127
}

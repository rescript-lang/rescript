let caml_is_printable = c => {
  let code = String.charCodeAtUnsafe(c, 0)
  code > 31 && code < 127
}

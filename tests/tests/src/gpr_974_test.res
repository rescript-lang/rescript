let _ = {
  assert(Js.Null_undefined.toOption(Js.Null_undefined.return("")) == Some(""))
  assert(Js.Undefined.toOption(Js.Undefined.return("")) == Some(""))
  assert(Js.Null.toOption(Js.Null.return("")) == Some(""))
}

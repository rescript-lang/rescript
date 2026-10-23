let _ = {
  assert(Nullable.toOption(Nullable.make("")) == Some(""))
  assert(Null.toOption(Null.make("")) == Some(""))
}

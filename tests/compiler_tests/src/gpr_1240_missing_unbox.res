let f = (x, y) => {
  let (x, y) = (ref(x), ref(y))
  (x.contents, y.contents)
}

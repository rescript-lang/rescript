let rec map = (f, x) =>
  switch x {
  | list{} => list{}
  | list{a, ...l} =>
    let r = f(a)
    list{r, ...map(f, l)}
  }

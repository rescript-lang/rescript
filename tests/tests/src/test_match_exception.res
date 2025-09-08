let f = (g, x) =>
  switch g(x) {
  | y => y
  | exception Not_found => 3
  }

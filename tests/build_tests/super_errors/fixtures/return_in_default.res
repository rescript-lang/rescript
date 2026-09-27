let f = () => {
  let g = (~x=%return(42)) => x
  g()
}

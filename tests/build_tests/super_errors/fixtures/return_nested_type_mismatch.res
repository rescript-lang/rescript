let f = () => {
  let g = (): string => %return(42)
  g()
}

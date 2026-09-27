let first = x => {
  if x > 0 {
    %return(x)
  }
  0
}
let nested = () => () => %return(())

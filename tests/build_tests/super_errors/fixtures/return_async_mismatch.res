let f = async x => {
  if x {
    %return("wrong")
  }
  42
}

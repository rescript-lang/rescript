let f = () => {
  module M = {
    let x = %return(42)
  }
  M.x
}

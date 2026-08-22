module A = {
  @res.hoistedFunction
  let hidden = () => "hidden"
  let visible = hidden
}

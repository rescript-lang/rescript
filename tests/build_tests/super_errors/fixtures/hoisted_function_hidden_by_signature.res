module A: {
  let visible: unit => string
} = {
  @res.hoistedFunction
  let hidden = () => "hidden"
  let visible = hidden
}

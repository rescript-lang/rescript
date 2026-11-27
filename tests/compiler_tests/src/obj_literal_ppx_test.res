let a = {
  module N = {
    @obj external create: (~x: 'a0, ~y: 'a1) => {"x": 'a0, "y": 'a1} = ""
  }
  N.create(~x=3, ~y=list{1, 2, 3})
}

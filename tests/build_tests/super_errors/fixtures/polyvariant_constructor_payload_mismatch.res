type test = [#Click(int)]
type test2 = [#Click(string)]
let a: test = #Click(1)
let b: test2 = a

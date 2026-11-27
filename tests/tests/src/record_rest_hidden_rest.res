/***
The interface exports `x` but not the rest bound next to it. `sum` is too
large for Lambda to inline, so its call is inlined in JS, which drops the last
read of `rest`: the destructuring must stay for `x`.
*/

type xy = {x: int, y: int}
type onlyY = {y: int}

let make: unit => xy = %raw(`() => ({x: 1, y: 2})`)

let {x, ...onlyY as rest} = make()

let sum = (_: onlyY, n) => {
  let total = ref(0)
  for i in 1 to n {
    total := total.contents + i
  }
  total.contents
}

let sumTo = n => sum(rest, n)

/* TODO: is it good or or bad to change arity of [f], 
   actually we can not, since we can not tell from the lambda layer
*/
let f = (h, (), x, y) => h(x, y)

let f = (h, ()) => {
  let u = 1 + 2
  Js.log(u)
  (x, y) => h(x, y)
}

open Mocha
open Test_utils

describe(__MODULE__, () => {
  test(__LOC__, () => {
    eq(__LOC__, 3, f(\"+", ())(1, 2))
  })
})

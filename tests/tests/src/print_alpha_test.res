/* TODO: is it good or or bad to change arity of [f], 
   actually we can not, since we can not tell from the lambda layer
*/
let f = (h, (), x, y) => h(x, y)

let f = (h, ()) => {
  let u = 1 + 2
  Js.log(u)
  (x, y) => h(x, y)
}

Mt.from_pair_suites(
  __MODULE__,
  {
    open Mt
    list{(__LOC__, _ => Eq(f(\"+", ())(1, 2), 3))}
  },
)

@inline let x = true

@inline let f = "hello"

@inline let f1 = `a`

@inline let f2 = `中文`
/* Do we need fix 
  let f2 : string = blabla
*/

module N: {
  @inline(`中文`) let f3: string
} = {
  @inline let f3 = `中文`
}

module N1 = () => {
  @inline let f4 = `中文`
  @inline let xx = 3e-6
  let xx0 = 3e-6
}
let h = f

let hh = f ++ f

open N

module H = N1()
open H
let (a, b, c, d, e) = (f, f1, f2, f3, f4)

@inline let f5 = true

@inline let f6 = 1

let () = Console.log((xx, xx0))

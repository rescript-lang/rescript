type rec t = A(t => int)

let g = x =>
  switch x {
  | A(v) => v(x)
  }

let loop = g(A(g))

let non_terminate = (
  x =>
    switch x {
    | A(v) => v(x)
    }
)(A(g))

type rec t0 = {xx: t0}
/* [@@unboxed] */

let rec xx = {xx: xx}

type rec t1 = A(array<t1>)

@unboxed type rec t2 = A2(array<t2>)

/* let rec h = A [|h|]     
let rec h1 = A [|h1|] // could be relaxed
let rec h2 = A2 [|h2|]  

;; Js.log (h,h2) */
/* If we inline g's definition -- it will be the same, inline uncarefully 
    (inline the inlined result)
    will make it non-terminating
*/

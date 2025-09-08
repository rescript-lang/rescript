/* such recursive value not allowed in 4.06 */

/* and us = 3 in */
/* Js.log us; */

/*
let second xs =
    let rec ys = 1 :: ys 
    and _zs () = List.hd xs 
    (* and us = 3 in  *) in

    xs 

let f x = 
    let f0 a = a+1 in 
    let f1 a  = f0 a + 1  in 
    let f2 a = f1 a + 1 in 
    f2 x    
*/

let rec even = {
  let odd = even
  n =>
    if n === 0 {
      true
    } else {
      odd(n - 1)
    }
}

let rec even2 = {
  let odd = n =>
    if n === 1 {
      true
    } else {
      even2(n - 1)
    }
  n =>
    if n === 0 {
      true
    } else {
      odd(n - 1)
    }
}

type t = {
  get: unit => int,
  set: int => unit,
}

let v = ref(0)
let rec obj = {
  get: _ => v.contents,
  set: i => v := i,
}

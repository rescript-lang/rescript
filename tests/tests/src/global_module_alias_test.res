open Belt

let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Eq(x, y)),
      ...suites.contents,
    }
}

module A = List

module B = A

module C = B

module D = C

module E = D

module F = E

/* module alias is reolved 
   B -> 0, any point to B will be ponited to A after type checking
*/
module type S = module type of List

let v = ref(0)

module Make = (U: S) => {
  let () = {
    incr(v)
    incr(v)
    incr(v)
  }
  include U
}

let f = () => {
  let () = {
    incr(v)
    incr(v)
    incr(v)
  }
  module G = F /* local module is not module alias */
  module H = G
  module(Make(H): S)
}

let () = eq(__LOC__, C.length(list{1, 2}), 2)

module H = Make(Make(Make(Make(F))))

let () = eq(__LOC__, v.contents, 12)

let g = () => {
  module A0 = List
  /* since module alias compiled to no code,  
     so whenever 
     {[ let module A0 = Global_module ]}
     happens we should just unpack it 
 */
  module A1 = A0
  module A2 = A1
  module A3 = A2
  A3.length(list{1, 2, 3, 4})
}

let xx = () => {
  module A0 = List
  /* since module alias compiled to no code,  
     so whenever 
     {[ let module A0 = Global_module ]}
     happens we should just unpack it 
 */
  module A1 = A0
  module A2 = A1
  module A3 = A2
  module(Make(A3): S)
}

let () = eq(__LOC__, g(), 4)

let () = {
  module V = unpack(xx(): S) /* xx () not inlined in 4.06 */
  eq(__LOC__, V.length(list{1, 2, 3}), 3)
  eq(__LOC__, v.contents, 15)
  module H = unpack(f(): S)
  eq(__LOC__, H.length(list{1, 2}), 2)
  eq(__LOC__, v.contents, 21)
}

Mt.from_pair_suites(__MODULE__, suites.contents)

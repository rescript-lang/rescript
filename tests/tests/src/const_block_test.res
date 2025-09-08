module Array = Ocaml_Array

let a = [0., 1., 2.]
let b = [0, 1, 2]
let c = [0, 1, 2, 3, 4, 5]
let v = (0, 1, 2, 3, 4, 5)
let f = () => {
  a[0] = 3.0
  b[0] = 3
}

/** should not be inlined here 
        everytime we call [h ()], 
        it should share the same copy
     */
let h = () => c

/** should not be inlined here 
        everytime we call [h ()], 
        it should share the same copy
     */
let g = () => {
  f()
  Mt.Eq((a[0], b[0]), (3.0, 3))
}

let suites = list{
  ("const_block_test", g),
  (
    "avoid_mutable_inline_test",
    _ => {
      let v = h()
      let v2 = h()
      let () = {
        v[0] = 3
        v2[1] = 4
      }
      Eq([3, 4, 2, 3, 4, 5], v)
    },
  ),
}

Mt.from_pair_suites(__MODULE__, suites)

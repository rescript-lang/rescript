/** we should also allow js function call from an external js module 
    
*/
@val("console.log")
external log: 'a => unit = "%ignore"

let v = u => {
  log(u)
  u
}

module type OrderedType = {
  type t
  let compare: (t, t) => int
}

@val("t") external test_f: module(OrderedType) => unit = "update_dummy"

let v = u => test_f(module(Test_order))

module type X = module type of Test_order

let u = (v: module(X)) => v

let s = u(module(Test_order))

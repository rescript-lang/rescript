// Other modules call an external with a relative path through the value its
// module exports, since the path is relative to that module's file. Only the
// non-relative external is inlined.
let a = Relative_path.foo2(1)
let b = Relative_path.Nested.nested(2)
let c = Relative_path.bar
let d = Relative_path.notRelative(3)

module M = Relative_path.Make({
  let v = 4
})

let e = Relative_path_intf.declared(5)
let f = Relative_path_intf.asValue(6)
let g = Relative_path.Recursive.inRec(7)

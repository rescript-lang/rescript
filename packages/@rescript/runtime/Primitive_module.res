module Array = Primitive_array_extern
module Obj = Primitive_object_extern

external import: 'a => promise<'a> = "%import"

@@warning("-37") /* `Function` may be used in runtime */

type rec shape =
  | Function
  | Lazy
  | Class
  | Module(array<(shape, string)>)
  | Value(Obj.t)

module type Empty = {}

/** Note that we have to provide a drop in replacement, since compiler internally will
    spit out ("CamlinternalMod".[init_mod|update_mod] unless we intercept it 
    in the lambda layer
*/
let init = (loc: (string, int, int), shape: shape) => {
  let undef_module = _ => throw(Undefined_recursive_module(loc))
  let rec loop = (shape: shape, struct_: Obj.t, idx) =>
    switch shape {
    | Function => Obj.setField(struct_, idx, Obj.magic(undef_module))
    | Lazy => Obj.setField(struct_, idx, Obj.magic(undef_module))
    | Class => Obj.setField(struct_, idx, Obj.magic((undef_module, undef_module, undef_module, 0)))
    | Module(comps) =>
      let v = Obj.repr(module({}: Empty))
      Obj.setField(struct_, idx, v)
      let len = Array.length(comps)
      for i in 0 to len - 1 {
        let (shape, name) = Array.getUnsafe(comps, i)
        loop(shape, v, name)
      }
    | Value(v) => Obj.setField(struct_, idx, v)
    }
  let res = Obj.repr(module({}: Empty))
  let dummy_name = "dummy"
  loop(shape, res, dummy_name)
  Obj.getField(res, dummy_name)
}

/* Note the [shape] passed between [init_mod] and [update_mod] is always the same 
   and we assume [module] is encoded as an array
*/
let update = (shape: shape, o: Obj.t, n: Obj.t): unit => {
  let rec aux = (shape: shape, o, n, parent, i) =>
    switch shape {
    | Function => Obj.setField(parent, i, n)

    | Lazy
    | Class =>
      Obj.updateDummy(o, n)
    | Module(comps) =>
      for i in 0 to Array.length(comps) - 1 {
        let (shape, name) = Array.getUnsafe(comps, i)
        aux(shape, Obj.getField(o, name), Obj.getField(n, name), o, name)
      }
    | Value(_) => ()
    }
  switch shape {
  | Module(comps) =>
    for i in 0 to Array.length(comps) - 1 {
      let (shape, name) = Array.getUnsafe(comps, i)
      aux(shape, Obj.getField(o, name), Obj.getField(n, name), o, name)
    }
  | _ => assert(false)
  }
}

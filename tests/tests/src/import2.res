let a = import(Import_external.makeA)
let b = Import_external.makeA

/* the imported export name goes through JS identifier conversion, matching
 the emitted export (`case` is exported as `$$case`) */
let c = import(Export_keyword.case)

/* hoisted export names also go through JS identifier conversion:
   the cmj stores the raw flattened name (Operator$+), the module exports
   the converted one (Operator$$plus) */
let d = import(Hoisted_function_attr.Operator.\"+")

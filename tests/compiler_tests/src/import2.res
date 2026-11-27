let a = import(Import_external.makeA)
let b = Import_external.makeA

/* the imported export name goes through JS identifier conversion, matching
 the emitted export (`case` is exported as `$$case`) */
let c = import(Export_keyword.case)

/* a nested make is imported through its flat export */
let d = import(Flat_make_export.Two.Inner.make)

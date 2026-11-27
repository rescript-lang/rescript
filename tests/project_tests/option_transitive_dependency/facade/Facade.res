let variant = A.Variant.value
let record = A.Record.value
let alias = A.Alias.value
let abstract = A.Abstract.value
let unboxed = A.Unboxed.value

let abstractIsUndefined = A.Abstract.isUndefined
let unboxedIsUndefined = A.Unboxed.isUndefined

let optionalAbstract = (~value: option<A.Abstract.t>=?) => value
let optionalUnboxed = (~value: option<A.Unboxed.t>=?) => value

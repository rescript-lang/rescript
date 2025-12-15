@genType
let useGetProp = (x: ImportJsValue.AbsoluteValue.t) => x->ImportJsValue.AbsoluteValue.getProp + 1

@genType
let useTypeImportedInOtherModule = (x: ImportJsValue.stringFunction) => x


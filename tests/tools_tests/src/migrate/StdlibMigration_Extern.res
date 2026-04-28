// Exercise migrations from Js_extern to new Stdlib APIs

let isNullish = Js_extern.testAny(%raw("null"))
let n = Js_extern.null
let u = Js_extern.undefined
let ty = Js_extern.typeof("hello")

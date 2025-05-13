type test = {x: int}

let x1 = Some("hello")
let x2 = Some(1)
let x3 = Some(Ok("hi"))
let x4 = Some(#polyvar)
let x5 = Some({x: 42})
let x6 = Some({"x": 42})
let x7 = Some([1, 2, 3])
let x8 = Some(() => ())

let x10 = Some(Nullable.null)
let x11 = Some(Nullable.undefined)
let x12 = Some(Nullable.Value("test"))

let x20 = Some(Jsx.null)
let x21 = Some(Date.make())
let x22 = Some(/test/)
let x23 = Some((Map.make(): Map.t<string, string>))
let x24 = Some((Set.make(): Set.t<string>))
let x25 = Some((WeakMap.make(): WeakMap.t<string, string>))
let x26 = Some((WeakSet.make(): WeakSet.t<string>))
let x27 = Some(Float32Array.fromArray([1.0]))
let x28 = Some(Symbol.make(""))
let x29 = Some(JsError.make(""))
let x30 = Some(ArrayBuffer.make(0))

let x98 = Some(list{})
let x99 = Some((Dict.make(): dict<string>))

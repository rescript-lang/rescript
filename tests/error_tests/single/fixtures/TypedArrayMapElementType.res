let v = (Int32Array.fromArray([1, 2, 3]) :> TypedArray.t<int>)
let s = v->TypedArray.map(x => Int.toString(x))

let optFn = (~x: option<int>=?) => x

let t = Some(1)

let f = optFn(~x=t)

let o1 = Option.getExn(Some(3))
let o2 = Option.mapWithDefault(Some(3), 0, x => x + 1)
let o3 = Option.getWithDefault(None, 0)

let r1 = Result.getExn(Ok(1))
let r2 = Result.mapWithDefault(Ok(1), 0, x => x + 1)
let r3 = Result.getWithDefault(Error("e"), 0)

let n1 = Null.getExn(Null.make(3))
let n2 = Null.getWithDefault(Null.null, 0)
let n3 = Null.mapWithDefault(Null.make(3), 0, x => x)

let nb1 = Nullable.getExn(Nullable.make(3))
let nb2 = Nullable.getWithDefault(Nullable.null, 0)
let nb3 = Nullable.mapWithDefault(Nullable.make(3), 0, x => x)

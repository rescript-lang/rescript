@unboxed type rec t = unknown = Unknown(_): t

let some = (x: unknown) => Some(x)

let some2 = x => Some(Unknown(x))

let h = [Unknown(3), Unknown(2), Unknown(Some(Unknown(2)))]

@unboxed type rec t0 = Any(_): t0

@unboxed type rec t1 = t0 = Any(_): t1

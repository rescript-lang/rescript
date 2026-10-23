@genType @unboxed
type v1 = A(int)

@genType @unboxed
type v2 = A(int)

@genType
let testV1 = (x: v1) => x

@genType @unboxed
type r1 = {x: int}

@genType @unboxed
type r2 = B({g: string})

@genType
let r2Test = (x: r2) => x


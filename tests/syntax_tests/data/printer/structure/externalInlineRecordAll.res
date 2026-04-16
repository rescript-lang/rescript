@module("m1")
external f1: (~a: {x: int}, ~b: {y: string}) => unit = "f1"

@module("m2")
external f2: (~opt: {z: int}=?) => unit = "f2"

@module("m3")
external f3: (~options: {
  misc?: {
    details: {
      n: int
    }
  }
}) => unit = "f3"

@module("m4")
external f4: int => {id2: string} = "f4"

// Non-arrow external should not derive inline records
@val
external s1: {...user, "age": int} = "s1"

@unboxed type rec kind = Empty | Again(kind)
@unboxed
type t = Recursive({kind: kind, value: int}) | Flag({kind: bool, reason: string})

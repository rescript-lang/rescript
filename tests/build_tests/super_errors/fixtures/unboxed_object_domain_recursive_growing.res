@unboxed type rec kind<'a> = Empty | Again(kind<(int, 'a)>)
@unboxed
type t = Recursive({kind: kind<string>, value: int}) | Flag({kind: bool, reason: string})

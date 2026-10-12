type text = {kind: option<string>, value: int}
type flag = {kind: bool, reason: string}
@unboxed type t = Text(text) | Flag(flag)

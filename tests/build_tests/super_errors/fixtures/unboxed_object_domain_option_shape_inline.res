type kind = None | Some({value: int})
@unboxed
type t = Value({kind: kind, value: int}) | Flag({kind: bool, reason: string})

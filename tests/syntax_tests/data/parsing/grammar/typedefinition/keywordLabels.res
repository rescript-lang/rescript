type t = {type: string, mutable open: bool, in?: int, let?: string}
type t = {mutable: bool, mutable?: int, mutable mutable: string}
type t = A({type: string, @as("x") for: int})
type t = {...r, type: string}
type t = {@attr type: string}
type tag = [#type | #module | #true | #false]
type tag = [> #type]
type fn = (~type: string, ~in: int=?) => unit
type fn = ~type: string => unit
external f: (~type: string, ~for: int=?) => unit = "f"

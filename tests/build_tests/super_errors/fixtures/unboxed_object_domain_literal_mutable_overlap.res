@unboxed type special = | @as("special") Special
@unboxed type t = Broad({kind: string, mutable value: int}) | Narrow({kind: special, value: int})

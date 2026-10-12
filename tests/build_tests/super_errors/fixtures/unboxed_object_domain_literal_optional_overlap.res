@unboxed type special = | @as("special") Special
@unboxed type t = Broad({kind: string, value: int}) | Narrow({kind: special, value?: int})

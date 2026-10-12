@unboxed type wrapped<'a> = Wrap({value: 'a})
@unboxed
type t = Wrapped({kind: wrapped<string>, value: int}) | Bare({kind: string, value: int})

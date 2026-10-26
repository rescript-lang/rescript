module Target: {
  type t = {value: int}
  type options = {step: int}
  exception Failed(string)
  let make: (int, ~start: int=?) => t
  let fromOptions: (int, ~options: options=?) => t
  let delay: (unit => int) => t
}

@deprecated({
  reason: "Use `Target.t` instead",
  migrate: %replace.type(: DeprecatedTemplates.Target.t),
})
type t = Target.t

type exn +=
  | @deprecated({
    reason: "Use `Target.Failed` instead",
    migrate: %replace.constructor(DeprecatedTemplates.Target.Failed),
  })
  Failed(string)

@deprecated({
  reason: "Use `Target.make` instead",
  migrate: DeprecatedTemplates.Target.make(~start=0),
})
let makeFromZero: int => Target.t

@deprecated({
  reason: "Use `Target.fromOptions` instead",
  migrate: DeprecatedTemplates.Target.fromOptions(~options=%insert.unlabelledArgument(1)),
})
let fromOptionsPositional: (int, Target.options) => Target.t

@deprecated({
  reason: "Use `Target.delay` instead",
  migrate: DeprecatedTemplates.Target.delay(() => %insert.unlabelledArgument(0)),
})
let ofValue: int => Target.t

@deprecated({
  reason: "Use the `+` operator instead",
  migrate: %insert.unlabelledArgument(0) + %insert.unlabelledArgument(1),
})
let add: (int, int) => int

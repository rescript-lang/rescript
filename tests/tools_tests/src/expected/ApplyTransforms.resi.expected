type task<'a>

@deprecated({
  reason: "Use `makeTask` instead.",
  migrate: ApplyTransforms.makeTask(
    @apply.transforms(["labelledToUnlabelledArgumentsInFnDefinition"])
    %insert.unlabelledArgument(0),
  ),
})
let make: ((~resolve: int => unit, ~reject: string => unit) => unit) => task<int>

let makeTask: ((int => unit, string => unit) => unit) => task<int>

@deprecated({
  reason: "Use `sum` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  ApplyTransforms.sum(~a=%insert.labelledArgument("x"), ~b=%insert.labelledArgument("y")),
})
let add: (~x: int, ~y: int, unit) => int

let sum: (~a: int, ~b: int) => int

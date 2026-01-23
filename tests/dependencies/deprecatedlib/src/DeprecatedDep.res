@deprecated({
  reason: "Use `newThing` instead.",
  migrate: DeprecatedDep.newThing(),
})
let oldThing = () => 1

let newThing = () => 2

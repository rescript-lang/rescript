open Belt.Option

let defaultZeroF = maybeFloat => mapWithDefault(maybeFloat, 0., a => a)
let defaultZeroI = maybeInt => mapWithDefault(maybeInt, -1, a => a)
let defaultZeroS = maybeString => mapWithDefault(maybeString, "0", a => a)

let dummyTimes2 = (a: int) => a * 2

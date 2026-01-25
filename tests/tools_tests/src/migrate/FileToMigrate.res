let someNiceString = DeprecatedStuff.slice("abcdefg", ~from=2, ~to_=5)

let someNiceString2 = DeprecatedStuff.slice(
  DeprecatedStuff.slice("abcdefg", ~from=0, ~to_=1),
  ~from=2,
  ~to_=5,
)

let someNiceString3 = "abcdefg"->DeprecatedStuff.slice(~from=2, ~to_=5)

let shift1 = DeprecatedStuff.shift([1, 2, 3])
let shift2 = [1, 2, 3]->DeprecatedStuff.shift

let deprecatedThing1 = DeprecatedStuff.deprecatedThing

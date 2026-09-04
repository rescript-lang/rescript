let f = ref(ignore)

f.contents = () => {
  doA()
  doB()
}
f := () => {
  doA()
  doB()
}

f.contents = @attr () => {
  doA()
  doB()
}
f := @attr () => {
  doA()
  doB()
}

f := () => doA()
f := @first @second () => doA()
f := async () => {
  await doA()
  doB()
}
f := (value: int): int => value + 1
f := (firstArgument, secondArgument, thirdArgument, fourthArgument, fifthArgument) => {
  doA(firstArgument, secondArgument)
  doB(thirdArgument, fourthArgument, fifthArgument)
}

// Keep comments on the assigned function and its body.
f := /* function */ () => {
  // body
  doA()
  doB()
}
f.contents = @attr /* attribute */ () => {
  doA()
  doB()
}
f := @attr () => {
  doA()
  doB()
} // assignment

// Explicit blocks and type constraints still need their delimiters.
f := {() => {
  doA()
  doB()
}}
f := (() => doA(): unit => unit)

// Other operators still need parentheses around functions.
let equal = f.contents == (() => {
  doA()
  doB()
})
let left = (() => doA()) == f.contents
let nested = (f := () => doA())->ignore

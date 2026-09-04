f := {
  first: one,
  second: two,
}
f.contents = {
  first: one,
  second: two,
}
obj["value"] = {
  first: one,
  second: two,
}

f := [
  firstVeryLongArrayElement,
  secondVeryLongArrayElement,
  thirdVeryLongArrayElement,
  fourthVeryLongArrayElement,
]
f.contents = [
  firstVeryLongArrayElement,
  secondVeryLongArrayElement,
  thirdVeryLongArrayElement,
  fourthVeryLongArrayElement,
]
obj["value"] = [
  firstVeryLongArrayElement,
  secondVeryLongArrayElement,
  thirdVeryLongArrayElement,
  fourthVeryLongArrayElement,
]

f := () => {
  doA()
  doB()
}
f.contents = () => {
  doA()
  doB()
}
obj["value"] = () => {
  doA()
  doB()
}

f := @attr () => {
  doA()
  doB()
}
f.contents = @attr () => {
  doA()
  doB()
}
obj["value"] = @attr () => {
  doA()
  doB()
}

f := switch value {
| Some(value) => value
| None => fallback
}
f.contents = switch value {
| Some(value) => value
| None => fallback
}
obj["value"] = switch value {
| Some(value) => value
| None => fallback
}

f := firstVeryLongOperand + secondVeryLongOperand + thirdVeryLongOperand + fourthVeryLongOperand
f.contents = firstVeryLongOperand + secondVeryLongOperand + thirdVeryLongOperand + fourthVeryLongOperand
obj["value"] = firstVeryLongOperand + secondVeryLongOperand + thirdVeryLongOperand + fourthVeryLongOperand

f := {firstVeryLongOperand + secondVeryLongOperand + thirdVeryLongOperand + fourthVeryLongOperand}
f.contents = {firstVeryLongOperand + secondVeryLongOperand + thirdVeryLongOperand + fourthVeryLongOperand}
obj["value"] = {firstVeryLongOperand + secondVeryLongOperand + thirdVeryLongOperand + fourthVeryLongOperand}

// Assignments used as operands retain their grouping.
let nested = (f := {first: one, second: two})->ignore
let field = (f.contents = {
  first: one,
  second: two,
})->ignore
let object = (obj["value"] = {
  first: one,
  second: two,
})->ignore

f := /* before value */ {
  // first field
  first: one,
  second: two, // second field
} // after assignment

let bracedObject = (obj["value"] = {
  firstVeryLongOperand + secondVeryLongOperand + thirdVeryLongOperand + fourthVeryLongOperand
})->ignore

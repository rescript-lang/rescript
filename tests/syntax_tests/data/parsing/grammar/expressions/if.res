if (foo) {
  true
} else {
  false
}

if foo == 2 {
  let bar = 1
  let foo = 2
  bar + foo
}

let ifThenElse = if foo {
   lala
} else {
  doStuff(x, y, z,)
}

let ifElseIfThen =
  if foo == bar {
    f()
  } else if foo == bar2 {
    f1()
  } else if foo == bar3 {
    f2()
  } else {
    f3()
  }

let x = if true { 1 } else { 2 } + if false { 2 } else { 3 }

// Basic
// if let Some(x) = foo() {
  // doSomethingWithX(x)
// }

// Else branch
// if let Some(x) = foo(){
  // doSomethingWithX(x)
// } else {
  // doSomethingElse()
// }

// Else-if support
// if let Some(x) = foo(){
  // doSomethingWithX(x)
// } else if let Some(y) = bar(){
  // doSomethingWithY(y)
// } else {
  // doSomethingElse()
// }

// Mixed conditions, pattern start
// if let Some(x) = foo(){
  // doSomethingWithX(x)
// } else if n > 10 {
  // doSomethingWithForN()
// } else {
  // doSomethingElse()
// }

// Mixed conditions, condition start
// if n > 10 {
  // doSomethingWithForN()
// } else if let Some(x) = foo() {
  // doSomethingWithX(x)
// } else {
  // doSomethingElse()
// }

// Maintains attrs correctly
// if n > 10 {
  // @aa
  // doSomethingWithForN()
// } else if let @bb Some(@cc x) = @dd foo() {
  // @ee
  // doSomethingWithY(x)
// } else {
  // @ff
  // doSomethingElse()
// }

// if let Some(x) = foo() {
  // doSomethingWithX(x)
// } else if let Some(y) = bar() {
  // doSomethingWithY(y)
// } else if let Some(z) = baz() {
  // doSomethingWithZ(z)
// }

// full destructuring
// if let Some(Thing(With({
  // many: Internal([Components, q]),
// }))) as p = foo() {
  // doSomethingWithE(e)
// }

// Assignment
// let a = if let Some(x) = foo() {
  // x
// } else {
  // 123
// }

// Nesting
// let getZ = nested =>
  // if let Some(point) = nested.origin {
    // if let Some(z) = point.z {
      // z
    // } else {
      // 0
    // }
  // } else {
    // 0
  // }

// Complex nesting
// if let Some(Thing(With({many: Internal([Components, q])}))) as p = foo() {
  // if let Other(Thing(With({many: Internal([Components, q])}))) as p = foo() {
    // doSomethingElse(e)
  // }
  // doSomethingWithE(e)
// } else if let Some(Thing(With({
  // many: Internal([Components, q]),
// }))) as p = foo() {
  // doSomethingWithE(e)
// } else if let Some(Thing(With({
  // many: Internal([Components, q]),
// }))) as p = foo() {
  // doSomethingWithE(e)
// }

// Mixed with ternary
// let a = b ? if let Some(x) = foo() {
  // 1
// } else {
  // 2
// } : 3

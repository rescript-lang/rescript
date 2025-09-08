let name = if true {
  user.name
}

let name = if true {
  user.name
} else {
  "steve"
}

let name = if true {
  user.name
} else if false {
  user.lastName
} else {
  defaultName
}

let () = if true {
  let a = 1
  let b = 2
  open Belt
  sideEffect()
  ()
} else {
  let a = 5
  let b = 6
  open React
  render()
  ()
}

let x = @attr if truth {
  sideEffect()
}

if inclusions[index] = (uid, url) {
  onChange(inclusions)
}


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

// Deep nesting
// let getZ = nested =>
  // if let Some(nested) = nested.a {
    // if let Some(nested) = nested.b {
      // if let Some(nested) = nested.c {
        // if let Some(nested) = nested.d {
          // if let Some(nested) = nested.e {
            // if let Some(nested) = nested.f {
              // if let Some(nested) = nested.g {
                // z
              // } else {
                // 0
              // }
            // } else {
              // 0
            // }
          // } else if let Some(nested) = nested.g {
            // z
          // } else {
            // 0
          // }
        // } else {
          // a ? b : c
        // }
      // } else {
        // 0
      // }
    // } else {
      // 0
    // }
  // } else {
    // 0
  // }

// Break testing
// if let Some(suuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuperLongName) = anotherSuuuuuuuuuuuuuuuuuuuuuuuuuuuperLongName() {
  // foo()
// }

// if let Some(suuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuperLongName) = anotherSuuuuuuuuuuuuuuuuuuuuuuuuuuuperLongNameanotherSuuuuuuuuuuuuuuuuuuuuuuuuuuuperLongName(withSomeArgsThatAreAlsoLarge, withManyArgsThatAreAlsoLarge) {
  // foo()
// }

// if let { suuuuuuuuuuuuuuuuuuuuuuuuuuuperLongName, suuuuuuuuuuuuuuuuuuuuuuuuuuuperLongName2, suuuuuuuuuuuuuuuuuuuuuuuuuuuperLongName3 } = buildMyRecord() {
  // foo()
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

// Ternary
// let a = b
  // ? if let Some(x) = foo() {
      // 1
    // } else {
      // 2
    // }
  // : 3

// let a = b
  // ? 1
  // : if let Some(x) = foo() {
      // 1
    // } else {
      // 2
    // }

// let a = b
  // ? if let Some(x) = foo() {
      // 1
    // } else {
      // 2
    // }
  // : if let Some(x) = foo() {
      // 1
    // } else {
      // 2
    // }

// print parens correct
// if let Some(x) = (3: option<int>) {
  // (x: int)
// } else {
  // (20: int)
// }

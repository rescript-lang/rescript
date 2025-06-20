// record
let r = {...expr, pexp_attributes: []}
let r = {a,}
let r = {a: expr}
let r = {a: expr,}
let r = {a: expr, b: expr2}
let r = {f: x => x + b}

// expression
let e = {a}
let e = {a;}
let e = {a; b()}

// unary expr
let e = {-a}
// binary expr
let e = {a + b}
// ternary
let e = {a ? true : false}
let e = {a->computation ? true : false}
// primary expr
let e = {a[0]}
let e = {f(b)}
let e = {a.b.c}
let e = {arr[x] = 20}

// es6 arrow
let e = {
  x => x + 1->doStuff(config)
}

let e = {
  (x => x + 1)->doStuff(config)
}

let e = {
  (x => x + 1) ? true : false
}


let e = {
  (x => x + 1)->sideEffect
  logToAnalytics(Shady.ml)
}

let f = {(event) => event.target.value}
let f = {(event) :string => event.target.value}

// block expression
let x = {
  let a = 1
  let b = 2
  a + b
}

// interpret strings correct
<> {"\n"->React.string} </>

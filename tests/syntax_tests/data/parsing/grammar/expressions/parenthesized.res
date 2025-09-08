let x = (1)
let f = (1.3)

let truth = (true)
let truth = (false)

let constructor = (None)

let longidentConstructor = (Option.None)

let txt = ("a string") 

let otherTxt = (`foo bar ${txt}`)

let ident = (myIdent)

let aList = (list{1, 2})

let anArray = ([1, 2])

let aTuple = ((1, 2))

let aRecord = ({name: "steve", age: 30})

let blockExpression = ({
  let a = 1
  let b = 2
  a + b
})

let assertSmthing = (assert(true))

let jsx = (<div className="cx"> foo </div>)

let ifExpr = (if true {
  Js.log(true)
} else {
  Js.log(false)
})

let forExpr = (for p in 0 to 10 { () })

let whileExpr = (while true {
  doSomeImperativeThing()
})

let switchExpr = (switch myVar {
  | Blue => "blue"
  | Red => "red"
})

let constrainedExpr = (x :int)

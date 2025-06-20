condition1 ? value1 : value2 

let x = condition1 ? value1 : value2 

let value = condition1
  ? value1
  : condition2
  ? value2
  : condition3
  ? value3
  : value4;

let paymentMessage = condition
    ? "Payment completed successfully"
    : state == "processing"
    ? "Payment processing"
    : state == "invalid_cvc"
    ? "There was an issue with your CVC number"
    : state == "invalid_expiry"
    ? "Expiry must be sometime in the past."
    : "There was an issue with the payment.  Please contact support.";

let paymentMessage =
  state == "success"
    ? "Payment completed successfully"
    : state == "processing"
    ? "Payment processing"
    : state == "invalid_cvc"
    ? "There was an issue with your CVC number"
    : state == "invalid_expiry"
    ? "Expiry must be sometime in the past."
    : "There was an issue with the payment.  Please contact support.";

let record = truth ? {x: 1, y: 2} : {x: 3, y: 5}
let record = truth ? {thisIsASuperLongFieldHere: loooooooooooooongIdentifier, anotherLongField: superLoooooooooooooooooongIdentifier} : {thisIsASuperLongFieldHere: loooooooooooooongIdentifier, anotherLongField: superLoooooooooooooooooongIdentifier}


let arr = truth ? [1, 2, 3, 4] : [4, 5, 6, 7]
let arr = truth ? [thisIsASuperLongIdentifieeeeeeeer, thisIsASuperLongIdentifieeeeeeeer2, thisIsASuperLongIdentifieeeeeeeer3] : [thisIsASuperLongIdentifieeeeeeeer, thisIsASuperLongIdentifieeeeeeeer2, thisIsASuperLongIdentifieeeeeeeer3]

let lst = truth ? list{1, 2, 3, 4} : list{4, 5, 6, 7}
let lst = truth ? list{thisIsASuperLongIdentifieeeeeeeer, thisIsASuperLongIdentifieeeeeeeer2, thisIsASuperLongIdentifieeeeeeeer3} : list{thisIsASuperLongIdentifieeeeeeeer, thisIsASuperLongIdentifieeeeeeeer2, thisIsASuperLongIdentifieeeeeeeer3}


let construct = truth ? Constructor(1, 2, 3, 4) : Constructor(4, 5, 6, 7)
let construct = truth ? Constructor(thisIsASuperLongIdentifieeeeeeeer, thisIsASuperLongIdentifieeeeeeeer2, thisIsASuperLongIdentifieeeeeeeer3) : Constructor(thisIsASuperLongIdentifieeeeeeeer, thisIsASuperLongIdentifieeeeeeeer2, thisIsASuperLongIdentifieeeeeeeer3)

let x = truth ? {let a = 1; let b = 2; a + b} : {let a = 1; let b = 2; a + b}
let x = truth
  ? {
      let a = 1;
      let b = 2;
      a + b
    }
  : {
      let a = 1;
      let b = 2;
      a + b
    }

let x = truth ? create(a, b) : create(b, c)
let x = truth ? create(thisIsASuperLongIdentifieeeeeeeer, thisIsASuperLongIdentifieeeeeeeer2, thisIsASuperLongIdentifieeeeeeeer3) : create(thisIsASuperLongIdentifieeeeeeeer, thisIsASuperLongIdentifieeeeeeeer2, thisIsASuperLongIdentifieeeeeeeer3)
let x = truth ? create(. a, b) : create(. b, c)
let x = truth ? create(. thisIsASuperLongIdentifieeeeeeeer, thisIsASuperLongIdentifieeeeeeeer2, thisIsASuperLongIdentifieeeeeeeer3) : create(. thisIsASuperLongIdentifieeeeeeeer, thisIsASuperLongIdentifieeeeeeeer2, thisIsASuperLongIdentifieeeeeeeer3)

let street = newYork ? streets[0] : streets[20]
let street = newYork ? thisIsASuperLongIdentifieeeeeeeer[0] : thisIsASuperLongIdentifieeeeeeeer[20]

let x = isNegative ? -x : y

let x = isGreater ? a + b : z - c
let x = isGreater ? thisIsASuperLongIdentifieeeeeeeer + 20  : thisIsASuperLongIdentifieeeeeeeer + 200

let x = isColor ? switch color {
  | Blue => ()
    | Red => ()
} : switch color {
  | Blue => ()
  | Red => ()
}

let x = exceptionLike ? try dangerousThing() catch {| Exit => () } : try dangerousThing() catch {| Exit => () }

let tuple = truth ? (1, 2, 3, 4) : (4, 5, 6, 7)
let tuple = truth ? (thisIsASuperLongIdentifieeeeeeeer, thisIsASuperLongIdentifieeeeeeeer2, thisIsASuperLongIdentifieeeeeeeer3) : (thisIsASuperLongIdentifieeeeeeeer, thisIsASuperLongIdentifieeeeeeeer2, thisIsASuperLongIdentifieeeeeeeer3)

let x = condition1 ? node.left : node.right 
let x = condition1 ? nodeWithVeryLooooooooooooooooongNameHere.left : nodeWithVeryLooooooooooooooooongNameHere.right 

let x = condition ? node.left = Some(newNode) : node.right = Some(newNode)
let x = condition1 ? nodeWithVeryLooooooooooooooooongNameHere.left = Some(newNode) : nodeWithVeryLooooooooooooooooongNameHere.right = Some(newNode)

let x = condition1 ? if true { () } else { () } : if true { () } else { () }
let () = condition1 ? {sideEffect(); sideEffect2()} : {sideEffect(); sideEffect2()}

let () = condition1 ? while true { doThing() } : while true { doOtherThing() }
let () = condition1 ? for i in 0 to 10 { () } : for i in 10 to 20 { () }

let () = (truth : bool) ? (10: int) : (20: int)

let () = condition ? {module L = Logger; L.log()} : {exception Exit; throw(Exit)}
let () = condition ? foo() : assert(false)

let x = condition ? module(Foo) : module(Int : Number)
let x = condition ? module(ModuleWithVeryLooooooooooooooooongNameHere) : module(ModuleWithVeryLooooooooooooooooongNameHereInt : Number)

let x = condition ? {open React; render()} : {open React; render() }
let x = condition ? %eval("console.log") : %eval("console.err")
let x = condition ? %eval("consoooooooooooooooooooooole.log") : %eval("consoleeeeeeeeeeeeeee.err")

let jsObj = condition ? {"x": 1, "y": 2} : {"x": 20, "y": 100}
let jsObj = truth ? {"thisIsASuperLongFieldHere": loooooooooooooongIdentifier, "anotherLongField": superLoooooooooooooooooongIdentifier} : {"thisIsASuperLongFieldHere": loooooooooooooongIdentifier, "anotherLongField": superLoooooooooooooooooongIdentifier}

let f = isPositive ? (x => x + 1) : (y => y - 1)
let f = isPositive ? ((a, b) : int => a + b) : (c, d) : int => c - d
let f = isPositive ? ((a, b) : int => a + b) : ((c, d) : int => c - d)

a => a ? () => {a} : () => {a}
a => a ? a : a
a => a ? aasdasdasdasdasdasdaaasdasdasdasdasdasdasdasdasdasdasdasdasdaaaaaaaaa : a

let x = @attrOnTernary (truth ? true : false)
let x = @attrOnCondition truth ? true : false

x ? y => () : onChange
let y = <input value onChange={x ? _ => () : onChange} />

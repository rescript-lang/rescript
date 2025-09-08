let r = #Reducer()
let r = #Reducer(state, nexState, sideEffect)
let r = #Reducer(superLongIdentiiiiiiiifffffffiiiiieeeeeeeer, superLongIdentiiiiiiiifffffffiiiiieeeeeeeer, superLongIdentiiiiiiiifffffffiiiiieeeeeeeer)
let r = #Reducer([state, nexState, sideEffect])
let r = #Reducer([
  superLongIdentiiiiiiiifffffffiiiiieeeeeeeer,
  superLongIdentiiiiiiiifffffffiiiiieeeeeeeer,
  superLongIdentiiiiiiiifffffffiiiiieeeeeeeer,
])

let r = #Reducer({state: nextState, sideEffects: []})

let c = #Constr({firstField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer, secondField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer, thirdField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer})

let forceBreak = #Cartesian({
  x: Omega.x,
  y: Theta.y
})

let c = #Constr(list{
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
  ...superLoooooooooooooooooooooooooooooongListHere,
})

let c = #Constr(list{
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
  superLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer,
})

let c = #ConstructorWithASuuuuuuuuuuuuuuuperLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer([])
let c = #ConstructorWithASuuuuuuuuuuuuuuuperLoooooooooooooooooooooooooooooongIiiiiiiiiideeeentifieeeeeeeeeeeeeeeeer(list{})


let coordinate = #JsCoord({"x": 1, "y": 1})
let user = #JsUser({
  "name": "steve",
  "age":  32
})

let rec peekMinNode = node =>
  switch node {
  | #None => #None
  | #Some(node) =>
    if node.left === #None {
      // this shouldn't break over multiple lines
      #Some(node)
    } else {
      node.left->peekMinNode
    }
  }

let rec peekMinNode = node =>
  switch node {
  | #None => #None
  | #Some(node) =>
    if node.left === #None {
      // this shouldn't break over multiple lines
      #MyConstructor(node, nodeLongIdentifiiiiieeeeeeer1, nodeLongIdentifiiiiieeeeeeer2)
    } else {
      node.left->peekMinNode
    }
  }

let constructor =
  @attr #Blue

let x =
  switch (x) {
  | #Bar =>
    ReasonReact.UpdateWithSideEffects(
      self => {
        let _ = 1;
        apply(bar);
      },
    )
  | #Foo => ()
  };

// tuple as single argument
let x = #Some((1, 2))

let math = if discriminant < 0. {
  #None
} else {
  #Some((
    (-.b -. Js.Math.sqrt(discriminant)) /. (2. *. a),
    (-.b +. Js.Math.sqrt(discriminant)) /. (2. *. a),
  ))
}

switch x {
| #...typevar => 42
}


let r = #\"Reducer⛪️"
let r = #\"type"(\"module", \"let")

let r = #lident
let r = #lident(a, b)
let r = #\"exotic lident"
let r = #\"exotic lident"(a, b)

let x = #"1"
let x = #"123"
let x = #"10space"
let x = #space10

let a = #"1"
let b = #"1a"
let c = #a2
let d = #abcd

#"BigBlue"(#"Shade-of-blue+++", #"Shade-of-blue---")

external openSync: (
  path,
  @string [
    | @as("r") #Read
    | @as("r+") #Read_write
    | @as("rs+") #Read_write_sync
    | @as("w") #Write
    | @as("wx") #Write_fail_if_exists
    | @as("w+") #Write_read
    | @as("wx+") #Write_read_fail_if_exists
    | @as("a") #Append
    | @as("ax") #Append_fail_if_exists
    | @as("a+") #Append_read
    | @as("ax+") #Append_read_fail_if_exists
  ],
) => unit = "openSync"

let x = #a(())
let x = #a()

let oneString = #1("payload")
let twoIntString = #2(3, "payload")

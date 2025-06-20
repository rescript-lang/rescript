switch x {
| A => ()
| B => ()
}

switch (a, b) {
| (Some(a), Some(b)) => 42
| _ => 3
}

let rec updateSum = (node, ~delta) =>
  switch node {
  | None => ()
  | Some(node) =>
    node.sum = node.sum +. delta
    node.parent->updateSum(~delta)
  }

let x = @attr switch x {
| Universe => ()
}

// #441 rescript format sometimes deletes comments
switch count {
// String in braces
| 1 => { "one" }
// Int in braces
| 2 => {2}
// Float in braces
| 3 => {3.}
// Bool in braces
| 4 => { true }
// Expression in braces
| 5 => { expr }
// Block
| 6 => {
    let _ = 123
    Js.Console.log("Must be block")
  }
}

switch route {
| A => <div> // div tag stays after >
    <div> {React.string("First A div")} </div>
    <div> {React.string("Second A div")} </div>
  </div>
| B => <> // fragment tag stays after <>
    <div> {React.string("First B div")} </div>
    <div> {React.string("Second B div")} </div>
  </>
}

switch x {
| A => {
    let _ = 1
    let _ = 2
  } // no blank line below
| B => ()
}

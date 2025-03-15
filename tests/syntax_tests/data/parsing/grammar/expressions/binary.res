node := if newBalance === 2 {
  avl->rotateRight(node)
} else {
  node 
}


// with attributes
node :=
  @attr
  if newBalance === 2 {
    avl->rotateRight(node)
  } else {
    node 
  }

let x = z |> switch z {| _ => false} 
let x = z |> @attr switch z {| _ => false} 
let x = z |> assert(z)
let x = z |> @attr assert(z)
let x = z |> try sideEffect() catch { | _ => f() }
let x = z |> @attr try sideEffect() catch { | _ => f() }
let x = z |> for i in 0 to 10 { () }
let x = z |> @attr for i in 0 to 10 { () }
let x = z |> while condition { () }
let x = z |> @attr while condition { () }

let x = a + -1 + -2
let x = a + @attr -1 + @attr -2
let x = a % a == 0
let x = a ^ a == 0

// should be interpreted as binary expression not prefix op
let x = a -b
let x = a -.b

// not binary expr
Constructor(a, b)
#Constructor(a, b)

let _ = {
  Constructor(a, b)
  #Constructor(a, b)
}

library.getBalance(. account)
->Promise.Js.catch(_ => {Promise.resolved(None)})

@@config({
  flags: [
    /* "-w";
     "@A" */
    /* "-drawlambda"; */
    /* "-dtypedtree"; */
    /* "-bs-diagnose"; */
    // "-dparsetree",
    /* "-dsource"; */
  ],
})
type t = {x: int, y: int}

// let f = (x,y) => {
//     let {} = {x,y}
//     x + y
// }

let f = (window, a, b) => {
  window["location"](. a, b)
}

// let h = () => {
//   // external hi : int => int = "hi"
//   let h = 3
//   h 

// }

type r = {
    x: int,
    @optional y: int,
    z : int 
}

let v0 = { x :  3 , z : 2 }




let v2 = { ... v0 , x : 3 }

let v1 : r = { x : 3 
  , z : 3
}

//@@warning("-56") // Turn off match case unreachable

let testMatch = v =>
  switch v {
  | {y} => y
//  | {y: @optional None} => 42
  }

let h = '😊'
let hey = "hello, 世界"
// failed to type check

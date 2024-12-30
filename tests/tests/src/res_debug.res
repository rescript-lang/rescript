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
  window["location"](a, b)
}

// let h = () => {
//   // external hi : int => int = "hi"
//   let h = 3
//   h

// }

type r = {
  x: int,
  y?: int,
  z: int,
}

let v0 = {x: 3, z: 2}

let v2 = {...v0, x: 3}

let v1: r = {
  x: 3,
  z: 3,
}

let testMatch = v =>
  switch v {
  | {y} => y
  | {y: ?None} => 42
  }

let h = '😊'
let hey = "hello, 世界"
// failed to type check

let optionMap = (x, f) =>
  switch x {
  | None => None
  | Some(v) => Some(f(v))
  }

type props<'name> = {key?: string, name?: string}

let name = None

let ok = {name: ?optionMap(name, x => x)}
let bad = {name: ?(name->optionMap(x => x))}

let identity = x => x

let name1 = Some("ReScript")

let ok1 = {name: ?identity(name1)}
let bad1 = {name: ?(name1->identity)}

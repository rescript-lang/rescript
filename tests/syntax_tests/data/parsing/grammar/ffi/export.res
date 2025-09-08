export type callback = ReactEvent.Mouse.t => unit

export type t = int and export s = string
export type t = int and s = string
type t = int and export s = string

export let callback = _ => Js.log("Clicked")
export callback = _ => Js.log("Clicked")

export let x = "hello world" and export y = 2
export x = "hello world" and export y = 2
let x = "hello world" and export y = 2

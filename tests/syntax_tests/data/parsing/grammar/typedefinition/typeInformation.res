// [type-equation]  [type-representation]  { type-constraint } 

// no type-equation, no type-representation
type t

type t = Module.t
type t = Module.t constraint 'a = int
type t = Module.t constraint 'a = int constraint 'b = string
type t = {a: int}
type t = {a: int} constraint 'a = int
type t = {a: int} constraint 'a = int constraint 'b = string
type t = Module.t = {a: int}
type t = Module.t = {a: int} constraint 'a = int
type t = Module.t = {a: int} constraint 'a = int constraint 'b = string

type t = node 
type t = node constraint 'a = int
type t = node constraint 'a = int constraint 'b = string
type t = node = {a: int}
type t = node = {a: int} constraint 'a = int
type t = node = {a: int} constraint 'a = int constraint 'b = string

// constructor declaration
type t = Red
type t = Red constraint 'a = string
type t = Red constraint 'a = string constraint 'b = int
type t = Red | Blue
type t = Red | Blue constraint 'a = string constraint 'b = int

type t = Red
type t = Red constraint 'a = string
type t = Red constraint 'a = string constraint 'b = int
type t = Red | Blue
type t = Red | Blue constraint 'a = string constraint 'b = int

type t = ..

type t = | Red
type t = | Red constraint 'a = string
type t = | Red constraint 'a = string constraint 'b = int
type t = | Red | Blue
type t = | Red | Blue constraint 'a = string constraint 'b = int

type t = private Green 
type t = private Green  constraint 'a = unit
type t = private Green  constraint 'a = unit constraint 'b = unit

type t = {x: int, y: int}

type callback = ReactEvent.Mouse.t => unit as 'callback
type callback = ReactEvent.Mouse.t => (unit as 'u)
type callback = (ReactEvent.Mouse.t => unit) as 'callback

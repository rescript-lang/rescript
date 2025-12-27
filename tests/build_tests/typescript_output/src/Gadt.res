// GADT - Generalized Algebraic Data Types
// Each constructor can have a different type parameter

// Simple GADT: number type witness
type rec number<_> =
  | Int(int): number<int>
  | Float(float): number<float>

// GADT for timezone subtyping
type standard
type daylight

type rec timezone<_> =
  | EST: timezone<standard>
  | EDT: timezone<daylight>
  | CST: timezone<standard>
  | CDT: timezone<daylight>

// GADT with payload and return type witness
type rec expr<_> =
  | IntLit(int): expr<int>
  | BoolLit(bool): expr<bool>
  | Add(expr<int>, expr<int>): expr<int>
  | Eq(expr<int>, expr<int>): expr<bool>
  | If(expr<bool>, expr<'a>, expr<'a>): expr<'a>

// Functions using GADTs
let add = (type a, x: number<a>, y: number<a>): a =>
  switch (x, y) {
  | (Int(x), Int(y)) => x + y
  | (Float(x), Float(y)) => x +. y
  }

let convertToDaylight = (tz: timezone<standard>): timezone<daylight> =>
  switch tz {
  | EST => EDT
  | CST => CDT
  }

// Using locally abstract types for polymorphic GADT functions
let rec eval:
  type a. expr<a> => a =
  expr =>
    switch expr {
    | IntLit(n) => n
    | BoolLit(b) => b
    | Add(a, b) => eval(a) + eval(b)
    | Eq(a, b) => eval(a) == eval(b)
    | If(cond, t, f) => eval(cond) ? eval(t) : eval(f)
    }

// Example values
let intNum: number<int> = Int(42)
let floatNum: number<float> = Float(3.14)
let estTz: timezone<standard> = EST
let edtTz: timezone<daylight> = EDT

let intExpr: expr<int> = IntLit(5)
let boolExpr: expr<bool> = BoolLit(true)
let addExpr: expr<int> = Add(IntLit(1), IntLit(2))
let eqExpr: expr<bool> = Eq(IntLit(1), IntLit(1))

// Function results
let addResult = add(Int(1), Int(2))
let daylightResult = convertToDaylight(EST)
let evalResult = eval(Add(IntLit(10), IntLit(20)))

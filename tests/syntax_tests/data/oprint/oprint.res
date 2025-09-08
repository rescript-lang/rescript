module Set = {
  module type OrderedType = {
    type rec t
    let compare : (t, t) => int
  }
  module type S = {
    type rec elt
    type rec t
    let empty : t
    let compare : (t, t) => int
  }
  module Make = (Ord:OrderedType) : (S with type elt = Ord.t) => {
    type rec elt = Ord.t
    type rec t
    let compare = assert(false)
    let empty = assert(false)
  }
}

module Hashtbl = {
  type t<'a, 'b>
  let create = (x:int) : t<_, _> => assert(false)
}

let name = "Steve"
let x = 42
let pi = 3.14

let numbersArray = [1, 2, 3, 4, 5]
let numbersTuple = (1, 2, 3, 4, 5)
let numbersList = list{1, 2, 3, 4, 5}

let add = (a:int, b:int) => 3

type s = string

type user = {
  name: string,
  age: int,
}

type user2 = {
  naaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaame: string,
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaage: int,
  emaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaail: string
}

module Diff = {
  let string = (s1: string, s2: string) => true
}

module Diff2 = Diff

type rec tree<'value> =
  | Nil
  | Node(tree<'value>, 'value, tree<'value>)

type intTree = tree<int>

type rec tree2<'value> =
  | Niiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiil2
  | Noooooooooooooooooooooooooooooooooooode2(tree2<'value>, 'value, tree2<'value>)

type rec tree3<'value> =
  | Nil3
  | Node3({left: tree3<'value>, value: 'value, right: tree3<'value>})

type rec tree4<'value> =
  | Nil4
  | Node4({leeeeeeeeeeeeeeeeeeeeeeeeeeeeft: tree3<'value>, vaaaaaaaaaaaaaaaaaaaaalue: 'value, riiiiiiiiiiiiiiiiiiiiiiight: tree3<'value>})

type color = ..

type color += Blue
type color += | Red | Green

type color += | Blaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaack | Oraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaange | Reeeeeeeeeeeeeeeeeeeed

module Expr = {
  type attr = ..

  type attr += private Str(string)

  type attr +=
    | Int(int)
    | Float(float)
}

module User = {
  type t = {name: string, age: int}
}

type userT = User.t = {name: string, age: int}

type veeeeeeeeeeeeeeeeeeeeeeeeryLongTypeAlias = string

type rgb = [#Red | #Green | #Blue]
type longRgb = [#Reeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeed | #Greeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeen | #Blueeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee | #Rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrgb(float, float, float) | #Rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrgb2(veeeeeeeeeeeeeeeeeeeeeeeeryLongTypeAlias, veeeeeeeeeeeeeeeeeeeeeeeeryLongTypeAlias, veeeeeeeeeeeeeeeeeeeeeeeeryLongTypeAlias)]
let red = #Red
let red2: rgb = #Red

let int = #Int(123)
let int2 = #Int("abc", true)
let int3 = #Int(1.0, 2.0, 3.0)

type customRgb = [#Rbg(float, float, float)]
let color = #Rbg(1.0, 0.6, 0.2)
let tupleAsSingleArg = #tuple((1.3, 4.2))
let oneArg = #One("oneArgument")

type redColor = [ #Red ]
type greenColor = [ #Green ]
type blueColor = [ #Blue ]
type rgbColor = [ redColor | greenColor | blueColor ]

module M = {
  type data = [#IntData(int) | #StrData(string)]
  let stringOfData = (x: data) =>
    switch x {
    | #IntData(i) => ""
    | #StrData(s) => s
    }
}
let y = M.stringOfData(#IntData(123))

type rgbw = [#Red | #Green | #Blue | #White ]

let id = (x: [> #Red | #Green | #Blue]) => x
let id = (x: [> rgb]) => x

type point = [ | #Point(float, float) ]
type shape = [ | #Rectangle(point, point) | #Circle(point, float) ]

let pi = 4.0
let computeArea = (s: shape) =>
  switch s {
  | #Rectangle(#Point(x1, y1), #Point(x2, y2)) => 1.0
  | #Circle(_, radius) => 1.0
  }

let shoelaceFormula = (#Point(x1:float, y1:float), #Point(x2:float, y2:float), #Point(x3:float, y3:float)) =>
  1.0

type shapePlus = [
| #Rectangle(point, point)
| #Circle(point, float)
| #Triangle(point, point, point)
]

let computeAreaPlus = (sp: shapePlus) =>
  switch sp {
  | #Triangle(p1, p2, p3) => shoelaceFormula(p1, p2, p3)
  | #...shape as s => computeArea(s)
  }

let computeAreaExotic = (sp) =>
  switch sp {
  | #"R-Triangle+"(_p1, _p2, _p3) => ()
  | #...shape as s => ()
  }

let top = #Point(3.0, 5.0)
let left = #Point(0.0, 0.0)
let right = #Point(3.0, 0.0)

let circ = #Circle(top, 3.0)
let tri = #Triangle(top, left, right)

let x = computeAreaPlus(circ)
let y = computeAreaPlus(tri)

let basic_color_to_int = color =>
  switch color {
  | #Black => 0
  | #Red => 1
  | #Green => 2
  | #Yellow => 3
  | #Blue => 4
  | #Magenta => 5
  | #Cyan => 6
  | #White => 7
  }

let color_to_int = color =>
  switch color {
  | #Basic(basic_color, weight) =>
    let base = switch weight {
    | #Bold => 8
    | #Regular => 0
    }
    basic_color_to_int(basic_color)
  | #RGB(r:int, g:int, b:int) => 16
  | #Gray(i:int) => 232
  }

module type Conjunctive = {
  type u1 = [ | #A | #B]
  type u2 = [ | #A | #B | #C]

  let f: [< | #T([< u2]) & ([< u2]) & ([< u1])] => unit
  let g: [< | #S&([< u2]) & ([< u2]) & ([< u1])] => unit
};

// exotic idents in poly-vars
type t20 = [#"type"]
type t21 = [#"va r ia nt"]
type t22 = [#"Variant ⛰"]
type \"let" = int
type \"type" = [ #"Point🗿"(\"let", float) ]
type t23 = [
  | #"1"
  | #"10space"
  | #"123"
]

type exoticUser = {
  \"let": string,
  \"type": float,
  \"Upper": int
}

type arity1a = (. ()) => int
type arity1b = (. int) => int
type arity2 = (. int, int) => int

type obj1<'a> = option<{"a": int}>
type obj2 = {"a": int}
type obj3 = {. "a": int}
type obj4 = {"a": int}
type obj5<'a> = {..} as 'a
type obj6 = {"a": int}
type obj7 = {. "a": int}
type obj8<'a> = {.. "a": int} as 'a
type obj9 = {"A": int}

type objUser = {"name": string, "age": int}
type objUserWithLongFields = {"name": string, "x": int, "age": int, "looooooongFiiiiiiiieeeeeeeldName": string, "anoooooooooooooooootherLongFiiiiiieeeeeeeldName": int}
type objUserWithUppsercaseKeys = {"NAME": string, "AGE": int}

type objectCoordinate =  {"x": float, "y": float}
type threeDimensionalCoordinate = {...objectCoordinate, "z": float}
type differentSpreadedCoordinate = {"z": float, ...objectCoordinate, "alpha": int}

type multiSpreadedCoordinate = {
  ...threeDimensionalCoordinate,
  "a": int,
  ...differentSpreadedCoordinate,
  "b": int
}

type dotdotObjectCoordinate<'a> =  {.. "suuuuuuuuuuuperLooooooooooooongFieldNaaaaaaaaaaaame": float, "suuuuuuuuuuuperLooooooooooooongFieldNaaaaaaaaaaaame2222222222222": float} as 'a

type permissions = [
   | #777
   | #644
 ]

 type numericPolyVarWithPayload = [
   | #1(string)
   | #2(int, string)
 ]

 let numericPolyVarMatch = switch #644 {
   | #777 => #1("payload")
   | #644 => #2(42, "test")
 }

let sort = (type s, module(Set: Set.S with type elt = s), l:list<s>) => l

let make_set = (type s, cmp) => {
  module S = Set.Make({
    type t = s
    let compare = cmp
  })
  module(S: Set.S with type elt = s)
}

type picture = string

module type DEVICE = {
  let draw: picture => unit
}

let devices: Hashtbl.t<string, module(DEVICE)> = Hashtbl.create(17)

module rec A: {
  type t =
    | Leaf(string)
    | Node(ASet.t)
  let compare: (t, t) => int
} = {
  type t =
    | Leaf(string)
    | Node(ASet.t)
  let compare = (t1, t2) =>
    switch (t1, t2) {
    | (Leaf(s1), Leaf(s2)) => 3
    | (Leaf(_), Node(_)) => 1
    | (Node(_), Leaf(_)) => -1
    | (Node(n1), Node(n2)) => 12
    }
}
and ASet: Set.S with type elt = A.t = Set.Make(A)

type emptyObject = {.}

let f = (~x=?, ~y as _) => x

type call = CleanStart

let f = (~a=1, ()) => 1

type opt = {x:int, y?: option<string>}

let secondOrder = f => f()
let thirdOrder = f => f(() => ())

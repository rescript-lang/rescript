open Mocha
open Test_utils
open Untagged_module_class_types

// A global class named like an export of the module, and a class read
// through the properties of a global
%%raw(`
globalThis.UntaggedCircle = class UntaggedCircle {}
globalThis.UntaggedScope = {Shapes: {Triangle: class Triangle {}}}
`)

@instanceOf("UntaggedCircle") type globalCircle
@scope(("UntaggedScope", "Shapes")) @instanceOf("Triangle") type triangle

@new external makeGlobalCircle: unit => globalCircle = "UntaggedCircle"

@new @scope(("UntaggedScope", "Shapes"))
external makeTriangle: unit => triangle = "Triangle"

@unboxed
type value =
  | Shape(shape)
  | Circle(circle)
  | Square(square)
  | Polygon(polygon)
  | GlobalCircle(globalCircle)
  | Triangle(triangle)
  | Text(string)
  // Checked after the classes, so that every class above is checked
  | Plain({"plain": bool})

let kind = value =>
  switch value {
  | Shape(_) => "shape"
  | Circle(_) => "circle"
  | Square(_) => "square"
  | Polygon(_) => "polygon"
  | GlobalCircle(_) => "global circle"
  | Triangle(_) => "triangle"
  | Text(text) => text
  | Plain(_) => "plain"
  }

describe(__MODULE__, () => {
  test("a class check imports the class from its module", () => {
    eq(__LOC__, "shape", kind(Shape(makeShape())))
    eq(__LOC__, "circle", kind(Circle(makeCircle())))
  })

  test("a class check reads the class through its scope", () => {
    eq(__LOC__, "square", kind(Square(makeSquare())))
    eq(__LOC__, "triangle", kind(Triangle(makeTriangle())))
  })

  test("a class check imports the default export of a module", () => {
    eq(__LOC__, "polygon", kind(Polygon(makePolygon())))
  })

  test("an export and a global of the same name are different classes", () => {
    eq(__LOC__, "global circle", kind(GlobalCircle(makeGlobalCircle())))
    eq(__LOC__, "text", kind(Text("text")))
    eq(__LOC__, "plain", kind(Plain({"plain": true})))
  })
})

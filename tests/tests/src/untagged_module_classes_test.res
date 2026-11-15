open Mocha
open Test_utils

// Classes imported with @module. None of them is a global, so these checks
// work only if they read the imported class.

@module("./untagged_module_classes.js") @instanceOf("Object3D") type object3D

@module("./untagged_module_classes.js") @instanceOf("Mesh")
type mesh = private object3D

@module("./untagged_module_classes.js") @instanceOf("Group")
type group = private object3D

// An export's property
@module("./untagged_module_classes.js") @instanceOf("Addons.Line2")
type line2 = private object3D

// The default export
@module("./untagged_module_classes.js") @instanceOf("default") type widget

@module("./untagged_module_classes.js") @new
external makeObject3D: unit => object3D = "Object3D"

@module("./untagged_module_classes.js") @new external makeMesh: unit => mesh = "Mesh"
@module("./untagged_module_classes.js") @new external makeGroup: unit => group = "Group"

@module("./untagged_module_classes.js") @scope("Addons") @new
external makeLine2: unit => line2 = "Line2"

@module("./untagged_module_classes.js") @new external makeWidget: unit => widget = "default"

// Subclasses come first in the checks, whatever the order of the cases
@unboxed
type shape = Other(object3D) | Mesh(mesh) | Group(group) | Line(line2)

let describeShape = shape =>
  switch shape {
  | Mesh(_) => "mesh"
  | Group(_) => "group"
  | Line(_) => "line"
  | Other(_) => "other"
  }

@unboxed
type thing = Widget(widget) | Object(object3D) | Name(string)

let describeThing = thing =>
  switch thing {
  | Widget(_) => "widget"
  | Object(_) => "object"
  | Name(name) => name
  }

// A global and an export with the same name are different classes
%%raw(`globalThis.Shared = class Shared {}`)

@module("./untagged_module_classes.js") @instanceOf("Shared") type importedShared
@instanceOf("Shared") type globalShared

@module("./untagged_module_classes.js") @new
external makeImportedShared: unit => importedShared = "Shared"

@new external makeGlobalShared: unit => globalShared = "Shared"

@unboxed
type shared = Imported(importedShared) | Global(globalShared) | Text(string)

let describeShared = shared =>
  switch shared {
  | Imported(_) => "imported"
  | Global(_) => "global"
  | Text(text) => text
  }

describe(__MODULE__, () => {
  test("a class imported from a module", () => {
    eq(__LOC__, describeShape(Other(makeMesh()->Obj.magic)), "mesh")
    eq(__LOC__, describeShape(Other(makeGroup()->Obj.magic)), "group")
    eq(__LOC__, describeShape(Other(makeObject3D())), "other")
  })

  test("a property of an export", () => {
    eq(__LOC__, describeShape(Other(makeLine2()->Obj.magic)), "line")
  })

  test("the default export", () => {
    eq(__LOC__, describeThing(Widget(makeWidget())), "widget")
    eq(__LOC__, describeThing(Object(makeObject3D())), "object")
    eq(__LOC__, describeThing(Name("name")), "name")
  })

  test("an export and a global of the same name", () => {
    eq(__LOC__, describeShared(Imported(makeImportedShared())), "imported")
    eq(__LOC__, describeShared(Imported(makeGlobalShared()->Obj.magic)), "global")
    eq(__LOC__, describeShared(Text("text")), "text")
  })
})

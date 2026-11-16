// Classes of a module, used as untagged variant cases in
// untagged_module_class_test.res

@module("./untagged_module_class_lib.mjs") @instanceOf("UntaggedShape")
type shape

@module("./untagged_module_class_lib.mjs") @instanceOf("UntaggedCircle")
type circle = private shape

@module("./untagged_module_class_lib.mjs") @scope("shapes") @instanceOf("Square")
type square = private shape

// The module's default export
@module("./untagged_module_class_lib.mjs") @instanceOf("default")
type polygon = private shape

@new @module("./untagged_module_class_lib.mjs")
external makeShape: unit => shape = "UntaggedShape"

@new @module("./untagged_module_class_lib.mjs")
external makeCircle: unit => circle = "UntaggedCircle"

@new @module("./untagged_module_class_lib.mjs") @scope("shapes")
external makeSquare: unit => square = "Square"

@new @module("./untagged_module_class_lib.mjs")
external makePolygon: unit => polygon = "default"

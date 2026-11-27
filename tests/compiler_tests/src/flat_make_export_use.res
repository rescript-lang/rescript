let top = Flat_make_export.make()

let one = Flat_make_export.One.make()
let oneKeep = Flat_make_export.One.keep()

let two = Flat_make_export.Two.Inner.make()
let twoKeep = Flat_make_export.Two.Inner.keep()

let three = Flat_make_export.Three.Inner.Deep.make()
let threeKeep = Flat_make_export.Three.Inner.Deep.keep()

let nested = Flat_make_export.Ambiguous.B.make()
let exoticValue = Flat_make_export.Ambiguous.\"B$make"()
let recursive = Flat_make_export.RecursiveA.make()
let coerced = Flat_make_export.Coerced.make()
let rebound = Flat_make_export.Rebound.make
let applied = Flat_make_export.Applied.make()
let included = Flat_make_export.Included.make()
let aliased = Flat_make_export.Aliased.make()
let hidden = Flat_make_export_hidden.A.B.make()
let hiddenBoth = Flat_make_export_hidden.both()
let reexported = Flat_make_export_reexport.Inner.make()
let reexportedDeep = Flat_make_export_reexport.Deep.Deep.make()

let awaited = async () => {
  module Lib = await Flat_make_export
  module Inner = Lib.Three.Inner
  (Lib.Two.Inner.make(), Inner.Deep.make())
}

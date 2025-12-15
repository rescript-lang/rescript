module OtherFile = ModuleAliases2
module OtherFileAlias = OtherFile

@genType
type record = OtherFile.record

@genType
type record2 = OtherFileAlias.record

module OuterAlias = OtherFile.Outer

@genType
type outer = OtherFileAlias.Outer.outer

@genType
type outer2 = OuterAlias.outer

module OtherFile1 = OtherFile
module Outer2 = OtherFile1.Outer
module Inner2 = Outer2.Inner

@genType
type my2 = Inner2.inner

@genType
type inner1 = OtherFile.InnerAlias.inner

@genType
type inner2 = OtherFile.Outer.Inner.inner

@genType
let testInner1 = (x: inner1) => x

@genType
let testInner1Expanded = (x: OtherFile.InnerAlias.inner) => x

@genType
let testInner2 = (x: inner2) => x

@genType
let testInner2Expanded = (x: OtherFile.Outer.Inner.inner) => x


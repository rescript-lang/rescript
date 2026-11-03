let callExternal = s => ThrowsDecl.parse(s)

let callExternalPipe = s => s->ThrowsDecl.parse

let callNested = x => ThrowsDecl.Inner.find(x)

let callIfaceLet = x => ThrowsIface.lookup(x)

let callIfaceExternal = s => ThrowsIface.parseIface(s)

module Alias = ThrowsDecl
let callAlias = s => Alias.parse(s)

let callOpen = s => {
  open ThrowsDecl
  parse(s)
}

module Included = {
  include ThrowsDecl
}

let callInclude = s => Included.parse(s)

module Applied = ThrowsDecl.Make()
let callFunctor = x => Applied.get(x)

let callFirstClass = (m: module(ThrowsDecl.S), x) => {
  module M = unpack(m)
  M.get(x)
}

@throws(JsExn)
let callExternalAnnotated = s => ThrowsDecl.parse(s)

let callViaLet = x => ThrowsDecl.viaLet(x)
let callViaLetIncluded = x => Included.viaLet(x)

module IfaceIncluded = {
  include ThrowsIface
}

let callImplOnlyIncluded = x => IfaceIncluded.implOnly(x)

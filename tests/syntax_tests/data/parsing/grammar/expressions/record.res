let r = {a: expr}
let r = {a: expr,} // trailing comma

let r = {Parsetree.pexp_attributes: [], Parsetree.loc: loc}

// punning
let r = {a, b, c} 
let r = {A.a, b}
let r = {A.a, b, C.c}

let r = {Parsetree.pexp_attributes, Parsetree.loc}
// trailing comma
let r = {Parsetree.pexp_attributes, Parsetree.loc,}

// with constraint
let r = {a: (expr : int), b: (x : string)}

// spread
let r = {...expr, pexp_attributes: []}
let r = {...expr, pexp_attributes: [], pexp_loc: loc}
let r = {...expr, pexp_attributes: [],} // trailing comma

// with type constraint on spread
let r = {...make() : myRecord, foo: bar}
let r = {...(make() : myRecord), foo: bar} // parens optional

let r = {x: ? None, y: ?None, z: ? (None:tt)}

let z = name => { name : ? name, x: 3}

let z = name => { ? name, x: 3}

let z = name => { name, ? x }

let zz = name => { name, ? x }

let _ = switch z {
  | {x: ? None, y: ? None, z: ? (None:tt)} => 11
  | {name:  ? name, x: 3} => 42
  | {? name, x: 3} => 4242
  | {x: ? None, y: ? None, z: ? (None:tt)} => 11
  | {name: ? name, x: 3} => 42
  | {? name, x: 3} => 4242
}

type tt = {x:int, @ns.opttinal y : string}

type ttt = {x:int, y?: string}

type multipleWithAttrs = {x:int, @attr y?: string}

type singleWithAttrs = {@attr y?: string}

type inlineWithAttrs = | A({@as("VALUE") value: string})
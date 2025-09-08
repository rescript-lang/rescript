let doc1: (/** ddd */ ~x: int) => int = (~x) => x + 1
let doc2: @res.doc(" ddd ") int => int = x => x + 1
let doc3: /** ddd */ int => int = x => x + 1
let doc4: (/** ddd */ ~x: int, /** eee */ n) => int = (~x) => x + 1

module M : {
    let foo : (/** xxdoc */ ~x:int, /** yydoc */ ~y:int) => int
} = {
    let foo= (~x, ~y) => x+y
}

@val
external ex : (/** ddd */ ~x: int, /** eee */ n) => int = "ex"

/** A */
module rec A : { type t } = {
  type t
}

/** B */ 
and B : { type t } = {
  type t
}

@res.doc(" C ")
and C : { type t} = {
  type t
}

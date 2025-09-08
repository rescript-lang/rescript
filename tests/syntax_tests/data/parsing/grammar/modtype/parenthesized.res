module type Bt = (Btree)
module type Bt = (Std.Btree)

module type Bt = @attrParens (@attrIdent Btree)

module type MyHash = {
  include (module type of { include Hashtbl })
  let replace: (t<'a, 'b>, 'a, 'b) => unit
}

module type MyHash = {
  include @onParens (@onModTypeOf module type of { include Hashtbl })
  let replace: (t<'a, 'b>, 'a, 'b) => unit
}

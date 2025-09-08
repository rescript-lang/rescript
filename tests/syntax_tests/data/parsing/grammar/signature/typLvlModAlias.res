module type Signature = {
  module Tree = Btree
  module Tree = Ns.Btree

  @attr
  module Tree = Btree
  @attr
  module Tree = Ns.Btree
}

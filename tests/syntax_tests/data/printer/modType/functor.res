module type Functor = SetLike => Set
module type Functor = (S: SetLike, B: BtreeLike) => NeoTree
module type Functor = (_: SetLike, _: BtreeLike) => NeoTree
module type Functor = (SetLike, BtreeLike) => NeoTree
module type Functor = SetLike => BtreeLike => NeoTree

module type Functor = (@attr1 _: SetLike, @attr2 _: BtreeLike) => NeoTree
module type Functor = @attr0 (@attr1 _: SetLike, @attr2 _: BtreeLike) => NeoTree


module type Functor = @attr1 SetLike => @attr2 BtreeLike => @attr3 NeoTree
module type Functor = @attr1 (SetLike) => @attr2 (BtreeLike) => @attr3 NeoTree

module type Functor = SetLike => Set with type t = A.t
module type Functor = SetLike => (Set with type t = A.t)

module type B = () => {

}
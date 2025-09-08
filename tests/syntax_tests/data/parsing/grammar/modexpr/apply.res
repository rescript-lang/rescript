module X = F(A)
module X = F(Arg1, Arg2, Arg3)
// trailing comma
module X = F(Arg1, Arg2, Arg3,)
// attributes
module X = @apply (@ident F)(@attr Arg1, @attr2 Arg2, @attr3 Arg3)
// curried style syntax
module X = F(Arg1)(Arg2)(Arg3,)

// generative functors
module X = F()
module X = F()()
module X = F((), ())

// constrained args
module X = F(A : SetLike)
module X = F(A : SetLike, B: TreeLike)
// trailing comma
module X = F(A : SetLike, B: TreeLike,)

let someFunctorAsFunction = (x: module(MT)): module(ResT) => module(SomeFunctor(unpack(x)))

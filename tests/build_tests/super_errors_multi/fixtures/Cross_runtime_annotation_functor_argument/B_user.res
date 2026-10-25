module Make = (
  X: {
    @instanceOf("Animal") type animal
    @instanceOf("Dog") type dog
  },
) => {
  @unboxed type t = A(X.animal) | D(X.dog)
}

module Applied = Make(A_classes)

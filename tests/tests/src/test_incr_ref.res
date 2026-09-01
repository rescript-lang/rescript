include (
  {
    let u = ref(0)
    let v = Int.Ref.increment(u)
  }: {
    let v: unit
  }
)

/* The reference is an expression, not a variable, so it has to be bound: it
 must be evaluated once, not once per mention. */
@val external mkRef: unit => ref<int> = "mkRef"

let onExpression = () => Int.Ref.increment(mkRef())
